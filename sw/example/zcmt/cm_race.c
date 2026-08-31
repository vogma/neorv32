// Zcmt branch-shadow race test
//
// A cm.jt placed directly behind a taken branch sits in the "branch shadow":
// the issue engine may detect it (and kick off the table read) before the
// fetch-restart from the branch arrives. The Zcmt sequencer must abort the
// sequence on the restart - the shadowed cm.jt must NEVER execute (poisoned
// table entry) and the pipeline must not wedge (the loops must complete).

#include <neorv32.h>
#include "zcmt.h"

static uint32_t race_table[64] __attribute__((aligned(64)));

volatile uint32_t zcmt_race_poison;
volatile uint32_t zcmt_race_resume;

// poison target: records the (buggy) execution, then recovers via a landing
// pad so the failure is reported instead of hanging
void __attribute__((naked, aligned(4))) zcmt_race_target(void)
{
  asm volatile(
      "la   t1, zcmt_race_poison \n"
      "li   t2, 1                \n"
      "sw   t2, 0(t1)            \n"
      "la   t1, zcmt_race_resume \n"
      "lw   t1, 0(t1)            \n"
      "jr   t1                   \n");
}

int cm_race(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- Zcmt branch-shadow race tests ---\n");

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);
  race_table[0] = (uint32_t)&zcmt_race_target; // poisoned entry
  asm volatile("fence.i" ::: "memory");
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)race_table);

  // unconditional-jump shadow
  neorv32_uart0_printf("cm.jt in j-shadow");
  zcmt_race_poison = 0;
  asm volatile(
      "la  t1, zcmt_race_resume \n"
      "la  t2, 9f               \n"
      "sw  t2, 0(t1)            \n"
      "li  t3, 100              \n"
      "2: j 3f                  \n"
      ".hword 0xA002            \n" // cm.jt 0 in the shadow - must never execute
      "3: addi t3, t3, -1       \n"
      "bnez t3, 2b              \n"
      "9:                       \n"
      ::: "t1", "t2", "t3", "memory");
  if (zcmt_race_poison == 0) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL (shadowed cm.jt executed)\n");
    result = 1;
  }

  // taken-conditional-branch shadow
  neorv32_uart0_printf("cm.jt in taken-branch shadow");
  zcmt_race_poison = 0;
  asm volatile(
      "la  t1, zcmt_race_resume \n"
      "la  t2, 9f               \n"
      "sw  t2, 0(t1)            \n"
      "li  t3, 100              \n"
      "2: beq zero, zero, 3f    \n" // always-taken conditional branch
      ".hword 0xA002            \n" // cm.jt 0 in the shadow - must never execute
      "3: addi t3, t3, -1       \n"
      "bnez t3, 2b              \n"
      "9:                       \n"
      ::: "t1", "t2", "t3", "memory");
  if (zcmt_race_poison == 0) {
    neorv32_uart0_printf(" - OK\n");
  } else {
    neorv32_uart0_printf(" - FAIL (shadowed cm.jt executed)\n");
    result = 1;
  }

  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}
