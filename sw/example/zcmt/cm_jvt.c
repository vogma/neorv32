// jvt CSR tests
//
// Zcmt enabled:  jvt @ 0x017 is WARL - base in bits 31:6, mode bits 5:0
//                hardwired to zero.
// Zcmt disabled: any jvt access and any cm.jt/cm.jalt encoding must raise
//                an illegal instruction exception (mcause = 2).

#include <neorv32.h>
#include "zcmt.h"

// ---------------------------------------------------------------------------
// WARL behavior (Zcmt enabled)
// ---------------------------------------------------------------------------
static int check_jvt_rw(uint32_t wdata, uint32_t expected)
{
  neorv32_cpu_csr_write(CSR_JVT, wdata);
  uint32_t rdata = neorv32_cpu_csr_read(CSR_JVT);
  if (rdata != expected) {
    neorv32_uart0_printf("  jvt: wrote 0x%x, expected 0x%x, got 0x%x\n", wdata, expected, rdata);
    return 0;
  }
  return 1;
}

int cm_jvt(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- jvt CSR tests ---\n");

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);

  neorv32_uart0_printf("jvt WARL (bits 5:0 hardwired zero)");
  int ok = 1;
  ok &= check_jvt_rw(0xFFFFFFFFu, 0xFFFFFFC0u);
  ok &= check_jvt_rw(0x00000000u, 0x00000000u);
  ok &= check_jvt_rw(0x1234567Fu, 0x12345640u);
  if (ok) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL\n");
    result = 1;
  }

  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}

// ---------------------------------------------------------------------------
// Illegal-when-disabled (Zcmt disabled)
// ---------------------------------------------------------------------------
static volatile uint32_t trap_count;

static void illegal_handler(void)
{
  trap_count++;
}

int cm_jvt_disabled(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- Zcmt illegal-when-disabled tests ---\n");

  neorv32_rte_handler_install(TRAP_CODE_I_ILLEGAL, illegal_handler);

  // jvt access must raise an illegal instruction exception
  neorv32_uart0_printf("jvt access traps illegal");
  trap_count = 0;
  neorv32_cpu_csr_read(CSR_JVT);
  if (trap_count == 1) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL (trap count %u)\n", trap_count);
    result = 1;
  }

  // cm.jt encoding must raise an illegal instruction exception
  neorv32_uart0_printf("cm.jt encoding traps illegal");
  trap_count = 0;
  asm volatile(".hword 0xA002\n" ::: "memory"); // cm.jt 0
  if (trap_count == 1) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL (trap count %u)\n", trap_count);
    result = 1;
  }

  neorv32_rte_handler_uninstall(TRAP_CODE_I_ILLEGAL);

  return result;
}
