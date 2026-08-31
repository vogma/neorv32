// Zcmt (table jump) extension test suite.
//
// The suite is runtime-gated on the mxisah CSR so the very same binary can be
// simulated with any RISCV_ISA_Zcmp/RISCV_ISA_Zcmt generic combination:
//  - Zcmt enabled:  run the functional tests (Zcmp-interplay parts self-skip
//                   when Zcmp is not implemented)
//  - Zcmt disabled: verify that jvt and cm.jt/cm.jalt raise illegal
//                   instruction exceptions

#include <neorv32.h>
#include "zcmt.h"

#define BAUD_RATE 1000000 // high baud rate to speed up simulation (use 19200 for real hardware)

int main()
{
  neorv32_rte_setup();

  // setup UART at default baud rate, no interrupts
  neorv32_uart0_setup(BAUD_RATE, 0);

  int fails = 0;

  if (neorv32_cpu_csr_read(CSR_MXISAH) & (1 << CSR_MXISAH_ZCMT)) {
    neorv32_uart0_printf("\n<<< Zcmt test suite (Zcmt implemented) >>>\n");
    fails += cm_jvt();
    fails += cm_jt();
    fails += cm_tbl_fault();
    fails += cm_tgt_fault();
    fails += cm_irq();
    fails += cm_race();
    fails += cm_umode();     // uses an unlocked PMP grant entry
    fails += cm_pmp_fault(); // locks PMP entries - keep last
  }
  else {
    neorv32_uart0_printf("\n<<< Zcmt test suite (Zcmt NOT implemented) >>>\n");
    fails += cm_jvt_disabled();
  }

  if (fails == 0) {
    neorv32_uart0_printf("\n[ZCMT] ALL TESTS PASSED\n");
  }
  else {
    neorv32_uart0_printf("\n[ZCMT] %i TEST(S) FAILED\n", fails);
  }

  return 0;
}
