// Zcmt jump-table read fault test
//
// Points jvt at unmapped address space and executes a cm.jalt. The table
// read (an implicit instruction fetch) gets a bus error, which must raise
// exactly one INSTRUCTION access fault with mepc = address of the
// cm.jalt itself and mtval = 0. Because the fault replaces the jalr
// micro-op with a faulting nop, ra must NOT be written (no link on fault).
// Afterwards the sequencer must have recovered: a normal cm.jalt against a
// valid table must still work.

#include <neorv32.h>
#include "zcmt.h"
#include "zcmt_trap.h"

static uint32_t recovery_table[64] __attribute__((aligned(64)));

int cm_tbl_fault(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- Zcmt table-read fault tests ---\n");

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);

  zt_install_handler();
  zt_reset();

  // point jvt at unmapped address space
  neorv32_cpu_csr_write(CSR_JVT, 0xA0000000u);

  neorv32_uart0_printf("cm.jalt table-read access-fault");

  uint32_t zcmt_pc = 0;
  uint32_t ra_after = 0;

  asm volatile(
      // set up landing pad for the trap handler
      "la  a0, 9f            \n"
      "la  a1, zt_resume     \n"
      "sw  a0, 0(a1)         \n"
      // ra sentinel - must survive the faulting cm.jalt
      "li  ra, 0x12345678    \n"
      "la  %[pc], 1f         \n"
      "1: .hword 0xA082      \n" // cm.jalt 32 - table read faults
      "9: mv %[raa], ra      \n"
      : [pc] "=&r"(zcmt_pc), [raa] "=r"(ra_after)
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
  if (zt_mepc != zcmt_pc) {
    neorv32_uart0_printf("\n  mepc: expected 0x%x (cm.jalt), got 0x%x", zcmt_pc, zt_mepc);
    ok = 0;
  }
  if (zt_mtval != 0) {
    neorv32_uart0_printf("\n  mtval: expected 0, got 0x%x", zt_mtval);
    ok = 0;
  }
  if (zt_ra != 0x12345678u) { // ra at trap entry
    neorv32_uart0_printf("\n  ra at trap: expected sentinel 0x12345678, got 0x%x (fault must not link!)", zt_ra);
    ok = 0;
  }
  if (ra_after != 0x12345678u) { // ra after resume
    neorv32_uart0_printf("\n  ra after resume: expected sentinel 0x12345678, got 0x%x", ra_after);
    ok = 0;
  }

  if (ok) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL\n");
    result = 1;
  }

  zt_restore_handler();

  // recovery: normal cm.jalt against a valid table must still work
  neorv32_uart0_printf("cm.jalt recovery after fault");
  recovery_table[32] = (uint32_t)&zcmt_target_link;
  asm volatile("fence.i" ::: "memory");
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)recovery_table);

  zcmt_hit_marker = 0;
  asm volatile(
      ".hword 0xA082 \n" // cm.jalt 32
      ::: "ra", "t1", "t2", "memory");

  if (zcmt_hit_marker == 0x000000C3) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL (marker 0x%x)\n", zcmt_hit_marker);
    result = 1;
  }

  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}
