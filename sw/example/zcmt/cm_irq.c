// Zcmt async interrupt (MTI) test
//
// Valid interrupt boundaries around a cm.jalt (verified against the NEORV32
// micro-architecture):
//  - mepc = A (the cm.jalt address): the IRQ was already pending at dispatch;
//    the instruction restarts transparently (the table read is side-effect
//    free).
//  - mepc = jump target: the IRQ arrived during the table-read wait; it is
//    buffered (system IRQs only fire in S_EXECUTE) and taken AFTER the
//    instruction completes - branch and ra-link included.
// mepc = A+2 must NEVER occur: cm.jalt jumps, it does not fall through.
// (This differs from Zcmp's cm.push restart semantics - do not compare.)
//
// The jump target does NOT return to A+2 (it jumps to a landing pad), so the
// code at A+2 is a poison zone that catches both a wrong fall-through and a
// wrong mepc.
//
// Approach: arm the machine timer with a tight deadline right before the
// cm.jalt and sweep the timer offset to cover all cycles of the sequence.

#include <neorv32.h>
#include "zcmt.h"

static uint32_t irq_table[64] __attribute__((aligned(64)));

// trap recording
static volatile uint32_t irq_mepc;
static volatile uint32_t irq_mcause;
static volatile uint32_t irq_count;

// target bookkeeping (referenced from naked asm - global)
volatile uint32_t zcmt_irq_hits;
volatile uint32_t zcmt_irq_resume;
volatile uint32_t zcmt_irq_poison;

static uint32_t irq_stack[64] __attribute__((aligned(16)));

// ---------------------------------------------------------------------------
// Jump target: counts the hit, then jumps to the landing pad (NOT ret - the
// code after the cm.jalt must stay unreachable)
// ---------------------------------------------------------------------------
void __attribute__((naked, aligned(4))) zcmt_irq_target(void)
{
  asm volatile(
      "la   t1, zcmt_irq_hits   \n"
      "lw   t2, 0(t1)           \n"
      "addi t2, t2, 1           \n"
      "sw   t2, 0(t1)           \n"
      "la   t1, zcmt_irq_resume \n"
      "lw   t1, 0(t1)           \n"
      "jr   t1                  \n");
}

// ---------------------------------------------------------------------------
// Minimal trap handler (naked): records mepc/mcause, disables MTI, returns
// via mret WITHOUT advancing mepc (mepc must be a restartable boundary)
// ---------------------------------------------------------------------------
static void __attribute__((naked, aligned(4))) zcmt_irq_handler(void)
{
  asm volatile(
      "csrrw sp, mscratch, sp    \n"
      "addi  sp, sp, -8          \n"
      "sw    a0, 0(sp)           \n"
      "sw    a1, 4(sp)           \n"
      "csrr  a0, mepc            \n"
      "la    a1, irq_mepc        \n"
      "sw    a0, 0(a1)           \n"
      "csrr  a0, mcause          \n"
      "la    a1, irq_mcause      \n"
      "sw    a0, 0(a1)           \n"
      "la    a1, irq_count       \n"
      "lw    a0, 0(a1)           \n"
      "addi  a0, a0, 1           \n"
      "sw    a0, 0(a1)           \n"
      // disable MTI (clear mie.MTIE = bit 7)
      "li    a0, 0x80            \n"
      "csrc  mie, a0             \n"
      "lw    a0, 0(sp)           \n"
      "lw    a1, 4(sp)           \n"
      "addi  sp, sp, 8           \n"
      "csrrw sp, mscratch, sp    \n"
      "mret                      \n");
}

static uint32_t saved_mtvec;

static void install_irq_handler(void)
{
  neorv32_cpu_csr_write(CSR_MSCRATCH, (uint32_t)&irq_stack[63]);
  saved_mtvec = neorv32_cpu_csr_read(CSR_MTVEC);
  neorv32_cpu_csr_write(CSR_MTVEC, (uint32_t)(&zcmt_irq_handler));
}

static void restore_rte_handler(void)
{
  neorv32_cpu_csr_write(CSR_MTVEC, saved_mtvec);
}

// ---------------------------------------------------------------------------
// Single trial: arm timer, execute one cm.jalt, classify
//
// Returns: 0 = inconclusive (no IRQ, or IRQ outside the sequence)
//          1 = IRQ landed on the cm.jalt boundary (mepc = A or target)
//         -1 = invalid state (mepc = A+2, poison executed, wrong cause, ...)
// ---------------------------------------------------------------------------
static int run_jalt_mti_trial(uint32_t timer_offset)
{
  uint32_t zcmt_pc = 0;

  irq_count = 0;
  irq_mepc = 0;
  irq_mcause = 0;
  zcmt_irq_hits = 0;
  zcmt_irq_poison = 0;

  asm volatile(
      // === Phase 1: setup (timer cannot fire yet) ===
      "la  a0, 9f                \n" // landing pad for the jump target
      "la  a1, zcmt_irq_resume   \n"
      "sw  a0, 0(a1)             \n"
      // read MTIME (0xFFF4BFF8) and compute deadline
      "lui a1, 0xfff4c           \n"
      "lw  a1, -8(a1)            \n"
      "add a1, a1, %[ofs]        \n"
      // prepare MTIMECMP[0] (0xFFF44000); hi = -1 prevents firing
      "lui a0, 0xfff44           \n"
      "li  t0, -1                \n"
      "sw  t0, 4(a0)             \n"
      "sw  a1, 0(a0)             \n"
      // enable MTIE + MIE (safe - MTIMECMP.hi still -1)
      "li  a1, 0x80              \n"
      "csrs mie, a1              \n"
      "li  a1, 0x08              \n"
      "csrs mstatus, a1          \n"
      "la  %[pc], 1f             \n"
      // === Phase 2: ARM timer, then immediately table-jump ===
      "sw  zero, 4(a0)           \n" // MTIMECMP[0].hi = 0 -> armed
      "1: .hword 0xA082          \n" // cm.jalt 32 -> zcmt_irq_target -> 9f
      // A+2 poison zone: only reachable by a wrong fall-through / wrong mepc
      "li  t1, 1                 \n"
      "la  t2, zcmt_irq_poison   \n"
      "sw  t1, 0(t2)             \n"
      // === Phase 3: epilogue ===
      "9:                        \n"
      "li  a0, 0x08              \n"
      "csrc mstatus, a0          \n"
      : [pc] "=&r"(zcmt_pc)
      : [ofs] "r"(timer_offset)
      : "a0", "a1", "t0", "t1", "t2", "ra", "memory");

  // disable MTI in case it has not fired yet
  neorv32_cpu_csr_clr(CSR_MIE, 1 << CSR_MIE_MTIE);

  if (zcmt_irq_poison != 0)
    return -1; // fall-through code executed
  if (zcmt_irq_hits != 1)
    return -1; // target must run exactly once per trial

  if (irq_count == 0)
    return 0; // no interrupt - inconclusive

  if (irq_mcause != TRAP_CODE_MTI)
    return -1;

  if (irq_mepc == (zcmt_pc + 2))
    return -1; // bug signature: invalid post-cm.jalt boundary

  if (irq_mepc == zcmt_pc)
    return 1; // trap-before: pending IRQ preempted the cm.jalt, restart
  if (irq_mepc == (uint32_t)&zcmt_irq_target)
    return 1; // trap-after: IRQ buffered during table read, taken at target

  return 0; // fired outside the cm.jalt boundary - inconclusive
}

// ---------------------------------------------------------------------------
// entry point
// ---------------------------------------------------------------------------
int cm_irq(void)
{
  int result = 0;
  int hits = 0;
  int failures = 0;

  neorv32_uart0_printf("\n--- Zcmt async interrupt (MTI) tests ---\n");

  if (!neorv32_clint_available()) {
    neorv32_uart0_printf("CLINT not available, skipping.\n");
    return 0;
  }

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);
  irq_table[32] = (uint32_t)&zcmt_irq_target;
  asm volatile("fence.i" ::: "memory");
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)irq_table);

  install_irq_handler();

  neorv32_uart0_printf("cm.jalt MTI boundaries (mepc = A or target)");

  for (uint32_t offset = 1; offset <= 30; offset++) {
    int trial = run_jalt_mti_trial(offset);
    if (trial == 1) {
      hits++;
    }
    else if (trial == -1) {
      failures++;
    }
  }

  if (failures > 0) {
    neorv32_uart0_printf(" - FAIL (%d/%d bad)\n", failures, hits + failures);
    result = 1;
  }
  else if (hits == 0) {
    neorv32_uart0_printf(" - SKIP (no interrupt hit the cm.jalt boundary)\n");
  }
  else {
    neorv32_uart0_printf(" - OK (%d hits)\n", hits);
  }

  restore_rte_handler();
  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}
