// Zcmt jump-target fetch fault test
//
// The table read succeeds but the table entry points into unmapped address
// space. The cm.jalt itself must complete (link ra = A+2, pc = target); the
// subsequent instruction fetch AT the target then raises the access fault:
// mcause = 1, mepc = target (distinguishes this fault point from a
// table-read fault, where mepc = A).

#include <neorv32.h>
#include "zcmt.h"
#include "zcmt_trap.h"

static uint32_t fault_table[64] __attribute__((aligned(64)));

#define BAD_TARGET 0xA0000000u

int cm_tgt_fault(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- Zcmt target-fetch fault test ---\n");

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);

  fault_table[32] = BAD_TARGET; // entry points into unmapped space
  asm volatile("fence.i" ::: "memory");
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)fault_table);

  zt_install_handler();
  zt_reset();

  neorv32_uart0_printf("cm.jalt target fetch access-fault");

  uint32_t zcmt_pc = 0;

  asm volatile(
      "la  a0, 9f            \n"
      "la  a1, zt_resume     \n"
      "sw  a0, 0(a1)         \n"
      "la  %[pc], 1f         \n"
      "1: .hword 0xA082      \n" // cm.jalt 32 -> jumps to BAD_TARGET, fetch there faults
      "9:                    \n"
      : [pc] "=&r"(zcmt_pc)
      :: "a0", "a1", "ra", "memory");

  int ok = 1;
  if (zt_count != 1) {
    neorv32_uart0_printf("\n  trap count: expected 1, got %u", zt_count);
    ok = 0;
  }
  if (zt_mcause != TRAP_CODE_I_ACCESS) {
    neorv32_uart0_printf("\n  mcause: expected 0x%x, got 0x%x", (uint32_t)TRAP_CODE_I_ACCESS, zt_mcause);
    ok = 0;
  }
  if (zt_mepc != BAD_TARGET) {
    neorv32_uart0_printf("\n  mepc: expected 0x%x (target), got 0x%x", (uint32_t)BAD_TARGET, zt_mepc);
    ok = 0;
  }
  if (zt_ra != (zcmt_pc + 2)) { // the cm.jalt completed, so it must have linked
    neorv32_uart0_printf("\n  ra at trap: expected 0x%x (A+2), got 0x%x", zcmt_pc + 2, zt_ra);
    ok = 0;
  }

  if (ok) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL\n");
    result = 1;
  }

  zt_restore_handler();
  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}
