// Zcmt user-mode test
//
// jvt is an unprivileged (URW) CSR and cm.jt/cm.jalt are not privileged
// instructions - both must work in U-mode. Since the testbench implements
// PMP regions, U-mode needs a matching PMP entry: one UNLOCKED full-address-
// space NAPOT region granting R/W/X (M-mode ignores unlocked entries).
//
// Flow: M-mode installs the grant + an ecall handler, enters U-mode via
// mret; the U-mode payload writes/reads jvt, executes cm.jalt 32 (link
// check) and cm.jt 0, then ecalls back to M-mode for evaluation.

#include <neorv32.h>
#include "zcmt.h"

// referenced from naked asm - must be global
uint32_t zcmt_um_table[64] __attribute__((aligned(64)));
volatile uint32_t zcmt_um_resume;
volatile uint32_t zcmt_um_mcause;
volatile uint32_t zcmt_um_jvt_rd;

static uint32_t um_stack[64] __attribute__((aligned(16)));

// ---------------------------------------------------------------------------
// U-mode payload (runs entirely in U-mode, no stack use)
// ---------------------------------------------------------------------------
void __attribute__((naked, aligned(4))) zcmt_umode_payload(void)
{
  asm volatile(
      // jvt write/read in U-mode (jvt is URW)
      "la   t0, zcmt_um_table   \n"
      "csrw 0x017, t0           \n" // jvt
      "csrr t1, 0x017           \n"
      "la   t2, zcmt_um_jvt_rd  \n"
      "sw   t1, 0(t2)           \n"
      // cm.jalt 32 in U-mode (link check via zcmt_target_link)
      ".globl zcmt_um_jalt      \n"
      "zcmt_um_jalt:            \n"
      ".hword 0xA082            \n"
      // cm.jt 0 in U-mode (leaves the final marker)
      "la   ra, 1f              \n"
      ".hword 0xA002            \n"
      "1:                       \n"
      // back to M-mode
      "ecall                    \n");
}

// ---------------------------------------------------------------------------
// Trap handler: records mcause, redirects to *zcmt_um_resume and returns to
// M-mode (sets mstatus.MPP = M before mret)
// ---------------------------------------------------------------------------
static void __attribute__((naked, aligned(4))) umode_trap_handler(void)
{
  asm volatile(
      "csrrw sp, mscratch, sp    \n"
      "addi  sp, sp, -8          \n"
      "sw    a0, 0(sp)           \n"
      "sw    a1, 4(sp)           \n"
      "csrr  a0, mcause          \n"
      "la    a1, zcmt_um_mcause  \n"
      "sw    a0, 0(a1)           \n"
      "la    a1, zcmt_um_resume  \n"
      "lw    a0, 0(a1)           \n"
      "csrw  mepc, a0            \n"
      // return to M-mode: mstatus.MPP = 11
      "li    a0, 0x1800          \n"
      "csrs  mstatus, a0         \n"
      "lw    a0, 0(sp)           \n"
      "lw    a1, 4(sp)           \n"
      "addi  sp, sp, 8           \n"
      "csrrw sp, mscratch, sp    \n"
      "mret                      \n");
}

extern char zcmt_um_jalt[];

int cm_umode(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- Zcmt user-mode tests ---\n");

  if (!(neorv32_cpu_csr_read(CSR_MISA) & (1 << 20))) { // misa.U
    neorv32_uart0_printf("U-mode not available, skipping.\n");
    return 0;
  }

  // U-mode needs a matching PMP entry when PMP regions are implemented;
  // unlocked full-space R/W/X grant (does not affect M-mode)
  if (neorv32_cpu_pmp_get_num_regions() > 0) {
    uint8_t grant = (PMP_NAPOT << PMPCFG_A_LSB) | (1 << PMPCFG_R) | (1 << PMPCFG_W) | (1 << PMPCFG_X);
    if (neorv32_cpu_pmp_configure_region(4, 0xFFFFFFFFu, grant) != 0) {
      neorv32_uart0_printf("PMP grant setup FAILED, skipping.\n");
      return 0;
    }
  }

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);

  zcmt_um_table[0]  = (uint32_t)&zcmt_target_mark1;
  zcmt_um_table[32] = (uint32_t)&zcmt_target_link;
  asm volatile("fence.i" ::: "memory");

  zcmt_um_mcause   = 0xFFFFFFFFu;
  zcmt_um_jvt_rd   = 0;
  zcmt_hit_marker  = 0;
  zcmt_captured_ra = 0;

  // install handler + handler stack
  neorv32_cpu_csr_write(CSR_MSCRATCH, (uint32_t)&um_stack[63]);
  uint32_t saved_mtvec = neorv32_cpu_csr_read(CSR_MTVEC);
  neorv32_cpu_csr_write(CSR_MTVEC, (uint32_t)(&umode_trap_handler));

  neorv32_uart0_printf("jvt + cm.jt/cm.jalt in U-mode");

  // enter U-mode at the payload, come back via ecall
  asm volatile(
      "la   a0, 9f              \n"
      "la   a1, zcmt_um_resume  \n"
      "sw   a0, 0(a1)           \n"
      "li   a0, 0x1800          \n" // mstatus.MPP = U
      "csrc mstatus, a0         \n"
      "la   a0, zcmt_umode_payload \n"
      "csrw mepc, a0            \n"
      "mret                     \n"
      "9:                       \n"
      ::: "a0", "a1", "ra", "t0", "t1", "t2", "memory");

  neorv32_cpu_csr_write(CSR_MTVEC, saved_mtvec);

  int ok = 1;
  if (zcmt_um_mcause != TRAP_CODE_UENV_CALL) { // payload must end with ecall-from-U
    neorv32_uart0_printf("\n  mcause: expected 0x%x (ecall from U), got 0x%x",
                         (uint32_t)TRAP_CODE_UENV_CALL, zcmt_um_mcause);
    ok = 0;
  }
  if (zcmt_um_jvt_rd != (uint32_t)zcmt_um_table) { // U-mode jvt write+read
    neorv32_uart0_printf("\n  jvt readback: expected 0x%x, got 0x%x",
                         (uint32_t)zcmt_um_table, zcmt_um_jvt_rd);
    ok = 0;
  }
  if (zcmt_captured_ra != ((uint32_t)zcmt_um_jalt + 2)) { // cm.jalt link in U-mode
    neorv32_uart0_printf("\n  cm.jalt ra: expected 0x%x, got 0x%x",
                         (uint32_t)zcmt_um_jalt + 2, zcmt_captured_ra);
    ok = 0;
  }
  if (zcmt_hit_marker != 0x000000A1) { // final marker from cm.jt 0
    neorv32_uart0_printf("\n  cm.jt marker: expected 0xA1, got 0x%x", zcmt_hit_marker);
    ok = 0;
  }

  if (ok) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL\n");
    result = 1;
  }

  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}
