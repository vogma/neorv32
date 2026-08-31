// Zcmt PMP permission tests
//
// The jump-table read is architecturally an INSTRUCTION fetch, so it needs
// PMP eXecute permission - read permission is irrelevant:
//  - phase A: locked X-only (no R) region over the table -> cm.jt must SUCCEED
//  - phase B: locked R+W (no X) region over a second table -> instruction
//    access fault with mepc = address of the cm.jt, mtval = 0
//
// NOTE: this test locks PMP entries (sticky until reset) - run LAST.

#include <neorv32.h>
#include "zcmt.h"
#include "zcmt_trap.h"

// two separate 64-byte tables, naturally aligned for 64-byte NAPOT regions
static uint32_t table_xonly[16] __attribute__((aligned(64)));
static uint32_t table_nox[16]   __attribute__((aligned(64)));

int cm_pmp_fault(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- Zcmt PMP permission tests (locks PMP - last) ---\n");

  if (neorv32_cpu_pmp_get_num_regions() < 4) {
    neorv32_uart0_printf("not enough PMP regions, skipping.\n");
    return 0;
  }

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);

  // fill both tables BEFORE locking (the locked regions also restrict M-mode)
  table_xonly[0] = (uint32_t)&zcmt_target_mark1;
  table_nox[0]   = (uint32_t)&zcmt_target_mark2;
  asm volatile("fence" ::: "memory");
  asm volatile("fence.i");

  // phase A region: locked NAPOT 64 bytes, eXecute only (no R, no W)
  uint32_t addr_a = (((uint32_t)&table_xonly[0]) >> 2) | 0x7u; // NAPOT, 64 bytes
  uint8_t  cfg_a  = (1 << PMPCFG_L) | (PMP_NAPOT << PMPCFG_A_LSB) | (1 << PMPCFG_X);
  // phase B region: locked NAPOT 64 bytes, R+W but no eXecute
  uint32_t addr_b = (((uint32_t)&table_nox[0]) >> 2) | 0x7u;
  uint8_t  cfg_b  = (1 << PMPCFG_L) | (PMP_NAPOT << PMPCFG_A_LSB) | (1 << PMPCFG_R) | (1 << PMPCFG_W);

  if ((neorv32_cpu_pmp_configure_region(2, addr_a, cfg_a) != 0) ||
      (neorv32_cpu_pmp_configure_region(3, addr_b, cfg_b) != 0)) {
    neorv32_uart0_printf("PMP region setup FAILED, skipping.\n");
    return 0;
  }

  zt_install_handler();

  // ---------------------------------------------------------------------
  // phase A: X=1/R=0 table -> cm.jt must succeed (execute permission suffices)
  // ---------------------------------------------------------------------
  neorv32_uart0_printf("table read with X=1/R=0 succeeds");
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)table_xonly);
  zt_reset();
  zcmt_hit_marker = 0;

  asm volatile(
      "la  a0, 9f            \n" // landing pad (in case of an unexpected trap)
      "la  a1, zt_resume     \n"
      "sw  a0, 0(a1)         \n"
      "la  ra, 1f            \n" // manual link
      ".hword 0xA002         \n" // cm.jt 0
      "1:                    \n"
      "9:                    \n"
      ::: "a0", "a1", "ra", "t1", "t2", "memory");

  if ((zt_count == 0) && (zcmt_hit_marker == 0x000000A1)) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL (traps %u, mcause 0x%x, marker 0x%x)\n", zt_count, zt_mcause, zcmt_hit_marker);
    result = 1;
  }

  // ---------------------------------------------------------------------
  // phase B: R=1/X=0 table -> instruction access fault, mepc = cm.jt address
  // ---------------------------------------------------------------------
  neorv32_uart0_printf("table read with R=1/X=0 faults");
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)table_nox);
  zt_reset();
  zcmt_hit_marker = 0;

  uint32_t zcmt_pc = 0;
  asm volatile(
      "la  a0, 9f            \n"
      "la  a1, zt_resume     \n"
      "sw  a0, 0(a1)         \n"
      "la  ra, 1f            \n"
      "la  %[pc], 1f         \n"
      "1: .hword 0xA002      \n" // cm.jt 0 - table read PMP-faults
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
  if (zt_mepc != zcmt_pc) {
    neorv32_uart0_printf("\n  mepc: expected 0x%x (cm.jt), got 0x%x", zcmt_pc, zt_mepc);
    ok = 0;
  }
  if (zt_mtval != 0) {
    neorv32_uart0_printf("\n  mtval: expected 0, got 0x%x", zt_mtval);
    ok = 0;
  }
  if (zcmt_hit_marker != 0) {
    neorv32_uart0_printf("\n  target executed despite fault (marker 0x%x)", zcmt_hit_marker);
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
