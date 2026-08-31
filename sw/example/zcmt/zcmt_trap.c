// shared minimal trap-recording machinery for the Zcmt fault tests
// (modeled on the Zcmp suite's cm_fetch_fault handler)

#include <neorv32.h>
#include "zcmt_trap.h"

volatile uint32_t zt_mepc;
volatile uint32_t zt_mcause;
volatile uint32_t zt_mtval;
volatile uint32_t zt_ra;
volatile uint32_t zt_count;
volatile uint32_t zt_resume;

// separate stack used exclusively by the trap handler
static uint32_t zt_stack[64] __attribute__((aligned(16)));

// ---------------------------------------------------------------------------
// Minimal trap handler (naked)
//
// Expects mscratch to hold a valid stack pointer. Records mepc, mcause,
// mtval and the trapped context's ra (x1), then redirects execution to
// *zt_resume via mret.
// ---------------------------------------------------------------------------
static void __attribute__((naked, aligned(4))) zt_handler(void)
{
  asm volatile(
      "csrrw sp, mscratch, sp    \n"
      "addi  sp, sp, -8          \n"
      "sw    a0, 0(sp)           \n"
      "sw    a1, 4(sp)           \n"
      // record trapped context's ra (x1) - still untouched here
      "la    a1, zt_ra           \n"
      "sw    ra, 0(a1)           \n"
      // record mepc
      "csrr  a0, mepc            \n"
      "la    a1, zt_mepc         \n"
      "sw    a0, 0(a1)           \n"
      // record mcause
      "csrr  a0, mcause          \n"
      "la    a1, zt_mcause       \n"
      "sw    a0, 0(a1)           \n"
      // record mtval
      "csrr  a0, mtval           \n"
      "la    a1, zt_mtval        \n"
      "sw    a0, 0(a1)           \n"
      // increment counter
      "la    a1, zt_count        \n"
      "lw    a0, 0(a1)           \n"
      "addi  a0, a0, 1           \n"
      "sw    a0, 0(a1)           \n"
      // redirect resume address
      "la    a1, zt_resume       \n"
      "lw    a0, 0(a1)           \n"
      "csrw  mepc, a0            \n"
      // restore and return
      "lw    a0, 0(sp)           \n"
      "lw    a1, 4(sp)           \n"
      "addi  sp, sp, 8           \n"
      "csrrw sp, mscratch, sp    \n"
      "mret                      \n");
}

static uint32_t zt_saved_mtvec;

void zt_install_handler(void)
{
  neorv32_cpu_csr_write(CSR_MSCRATCH, (uint32_t)&zt_stack[63]);
  zt_saved_mtvec = neorv32_cpu_csr_read(CSR_MTVEC);
  neorv32_cpu_csr_write(CSR_MTVEC, (uint32_t)(&zt_handler));
}

void zt_restore_handler(void)
{
  neorv32_cpu_csr_write(CSR_MTVEC, zt_saved_mtvec);
}

void zt_reset(void)
{
  zt_mepc   = 0;
  zt_mcause = 0;
  zt_mtval  = 0xCAFEBABEu;
  zt_ra     = 0;
  zt_count  = 0;
}
