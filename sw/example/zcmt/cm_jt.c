// cm.jt / cm.jalt functional tests
//
// Sets up a full 256-entry, 64-byte-aligned jump table (so even index 255 is
// an in-bounds read), points jvt at it and exercises the boundary indices:
//   cm.jt 0, cm.jt 31    (no link - the tests link manually via ra)
//   cm.jalt 32, cm.jalt 255 (hardware link, ra must be A+2)
//
// All hand-encoded control transfers live in single asm blocks with full
// clobber lists - GCC knows nothing about the calls these instructions make.
// NEORV32 is fixed little-endian, so the table entries are written as plain
// 32-bit little-endian words (the ISA distinguishes table-entry data
// endianness from instruction-fetch endianness; here they coincide).

#include <neorv32.h>
#include "zcmt.h"

// full 256-entry jump table, 64-byte aligned per jvt WARL definition
static uint32_t jump_table[256] __attribute__((aligned(64)));

// written by the jump targets
volatile uint32_t zcmt_hit_marker;
volatile uint32_t zcmt_captured_ra;

// ---------------------------------------------------------------------------
// Jump targets (naked - entered by cm.jt/cm.jalt, leave via ret)
// ---------------------------------------------------------------------------
void __attribute__((naked, aligned(4))) zcmt_target_mark1(void)
{
  asm volatile(
      "la   t1, zcmt_hit_marker \n"
      "li   t2, 0x000000A1      \n"
      "sw   t2, 0(t1)           \n"
      "ret                      \n");
}

void __attribute__((naked, aligned(4))) zcmt_target_mark2(void)
{
  asm volatile(
      "la   t1, zcmt_hit_marker \n"
      "li   t2, 0x000000B2      \n"
      "sw   t2, 0(t1)           \n"
      "ret                      \n");
}

void __attribute__((naked, aligned(4))) zcmt_target_link(void)
{
  asm volatile(
      "la   t1, zcmt_captured_ra \n"
      "sw   ra, 0(t1)            \n"
      "la   t1, zcmt_hit_marker  \n"
      "li   t2, 0x000000C3       \n"
      "sw   t2, 0(t1)            \n"
      "ret                       \n");
}

// ---------------------------------------------------------------------------
// Zcmp-interplay callee: entered via cm.jalt, uses cm.push/cm.popret and a
// nested cm.jalt 32 (-> zcmt_target_link, which clobbers ra; cm.popret
// restores it from the stack and returns to the original caller)
// ---------------------------------------------------------------------------
void __attribute__((naked, aligned(4))) zcmt_interplay_callee(void)
{
  asm volatile(
      ".hword 0xB842             \n" // cm.push {ra}, -16
      ".globl zcmt_interplay_jalt \n"
      "zcmt_interplay_jalt:      \n"
      ".hword 0xA082             \n" // nested cm.jalt 32 -> zcmt_target_link
      ".hword 0xBE42             \n" // cm.popret {ra}, 16
  );
}

// ---------------------------------------------------------------------------
// cm.jt: no link - link manually so the target's ret comes back to us
// ---------------------------------------------------------------------------
static int test_cm_jt(uint32_t marker_exp, const char *name, int idx0)
{
  zcmt_hit_marker = 0;

  if (idx0) { // cm.jt 0 (0xA002)
    asm volatile(
        "la   ra, 1f    \n" // manual link
        ".hword 0xA002  \n" // cm.jt 0
        "1:             \n"
        ::: "ra", "t1", "t2", "memory");
  }
  else { // cm.jt 31 (0xA07E)
    asm volatile(
        "la   ra, 1f    \n" // manual link
        ".hword 0xA07E  \n" // cm.jt 31
        "1:             \n"
        ::: "ra", "t1", "t2", "memory");
  }

  if (zcmt_hit_marker != marker_exp) {
    neorv32_uart0_printf("\n  %s: marker expected 0x%x, got 0x%x\n", name, marker_exp, zcmt_hit_marker);
    return 0;
  }
  return 1;
}

// ---------------------------------------------------------------------------
// cm.jalt: hardware link - target captures ra, must be A+2
// ---------------------------------------------------------------------------
static int test_cm_jalt(int idx255, const char *name)
{
  uint32_t zcmt_pc = 0;

  zcmt_hit_marker  = 0;
  zcmt_captured_ra = 0;

  if (idx255) { // cm.jalt 255 (0xA3FE) - exercises the table-address adder carry
    asm volatile(
        "la   %[pc], 1f \n"
        "1: .hword 0xA3FE \n" // cm.jalt 255
        : [pc] "=r"(zcmt_pc)
        :: "ra", "t1", "t2", "memory");
  }
  else { // cm.jalt 32 (0xA082) - lowest cm.jalt index
    asm volatile(
        "la   %[pc], 1f \n"
        "1: .hword 0xA082 \n" // cm.jalt 32
        : [pc] "=r"(zcmt_pc)
        :: "ra", "t1", "t2", "memory");
  }

  int ok = 1;
  if (zcmt_hit_marker != 0x000000C3) {
    neorv32_uart0_printf("\n  %s: marker expected 0xC3, got 0x%x\n", name, zcmt_hit_marker);
    ok = 0;
  }
  if (zcmt_captured_ra != (zcmt_pc + 2)) {
    neorv32_uart0_printf("\n  %s: ra expected 0x%x (A+2), got 0x%x\n", name, zcmt_pc + 2, zcmt_captured_ra);
    ok = 0;
  }
  return ok;
}

// ---------------------------------------------------------------------------
// Odd table entry: target address | 1 - hardware must clear bit 0 (pc = target & ~1)
// ---------------------------------------------------------------------------
static int test_odd_entry(void)
{
  zcmt_hit_marker = 0;

  asm volatile(
      "la   ra, 1f    \n" // manual link
      ".hword 0xA016  \n" // cm.jt 5 (table entry = zcmt_target_mark1 | 1)
      "1:             \n"
      ::: "ra", "t1", "t2", "memory");

  if (zcmt_hit_marker != 0x000000A1) {
    neorv32_uart0_printf("\n  odd entry: marker expected 0xA1, got 0x%x\n", zcmt_hit_marker);
    return 0;
  }
  return 1;
}

// ---------------------------------------------------------------------------
// minstret: a cm.jt must retire exactly one instruction. Self-calibrating:
// run two otherwise identical blocks, one jumping via plain "j", one via
// cm.jt 0 (both reach zcmt_target_mark1) - the minstret deltas must match.
// ---------------------------------------------------------------------------
static int test_minstret(void)
{
  uint32_t d_jal = 0, d_jt = 0;

  asm volatile( // reference block: plain jump (1 instruction)
      "csrr t3, minstret      \n"
      "la   ra, 1f            \n"
      "j    zcmt_target_mark1 \n"
      "1:                     \n"
      "csrr t4, minstret      \n"
      "sub  %[d], t4, t3      \n"
      : [d] "=r"(d_jal)
      :: "ra", "t1", "t2", "t3", "t4", "memory");

  asm volatile( // same block with the jump replaced by cm.jt 0
      "csrr t3, minstret      \n"
      "la   ra, 1f            \n"
      ".hword 0xA002          \n" // cm.jt 0
      "1:                     \n"
      "csrr t4, minstret      \n"
      "sub  %[d], t4, t3      \n"
      : [d] "=r"(d_jt)
      :: "ra", "t1", "t2", "t3", "t4", "memory");

  if (d_jt != d_jal) {
    neorv32_uart0_printf("\n  minstret: delta %u via cm.jt vs %u via j\n", d_jt, d_jal);
    return 0;
  }
  return 1;
}

// ---------------------------------------------------------------------------
// Zcmp interplay: cm.jalt 40 -> callee doing cm.push / nested cm.jalt 32 /
// cm.popret (only run when Zcmp is implemented)
// ---------------------------------------------------------------------------
extern char zcmt_interplay_jalt[];

static int test_interplay(void)
{
  zcmt_hit_marker  = 0;
  zcmt_captured_ra = 0;

  asm volatile(
      ".hword 0xA0A2  \n" // cm.jalt 40 -> zcmt_interplay_callee
      ::: "ra", "t1", "t2", "memory");

  int ok = 1;
  if (zcmt_hit_marker != 0x000000C3) { // set by the nested cm.jalt's target
    neorv32_uart0_printf("\n  interplay: marker expected 0xC3, got 0x%x\n", zcmt_hit_marker);
    ok = 0;
  }
  if (zcmt_captured_ra != ((uint32_t)zcmt_interplay_jalt + 2)) { // nested link value
    neorv32_uart0_printf("\n  interplay: nested ra expected 0x%x, got 0x%x\n",
                         (uint32_t)zcmt_interplay_jalt + 2, zcmt_captured_ra);
    ok = 0;
  }
  return ok;
}

// ---------------------------------------------------------------------------
// entry point
// ---------------------------------------------------------------------------
int cm_jt(void)
{
  int result = 0;

  neorv32_uart0_printf("\n--- cm.jt/cm.jalt functional tests ---\n");

  // build the jump table
  for (int i = 0; i < 256; i++) {
    jump_table[i] = (uint32_t)&zcmt_target_mark1; // safe default
  }
  jump_table[0]   = (uint32_t)&zcmt_target_mark1;
  jump_table[5]   = (uint32_t)&zcmt_target_mark1 | 1u; // odd entry - bit 0 must be cleared by HW
  jump_table[31]  = (uint32_t)&zcmt_target_mark2;
  jump_table[32]  = (uint32_t)&zcmt_target_link;
  jump_table[40]  = (uint32_t)&zcmt_interplay_callee;
  jump_table[255] = (uint32_t)&zcmt_target_link;
  asm volatile("fence.i" ::: "memory"); // table entries are fetched via the instruction bus

  uint32_t jvt_save = neorv32_cpu_csr_read(CSR_JVT);
  neorv32_cpu_csr_write(CSR_JVT, (uint32_t)jump_table);

  struct { const char *name; int ok; } tests[] = {
    { "cm.jt 0",              test_cm_jt(0x000000A1, "cm.jt 0", 1)   },
    { "cm.jt 31",             test_cm_jt(0x000000B2, "cm.jt 31", 0)  },
    { "cm.jalt 32",           test_cm_jalt(0, "cm.jalt 32")          },
    { "cm.jalt 255",          test_cm_jalt(1, "cm.jalt 255")         },
    { "odd entry (tgt & ~1)", test_odd_entry()                       },
    { "minstret (1 retire)",  test_minstret()                        },
  };

  for (unsigned i = 0; i < sizeof(tests)/sizeof(tests[0]); i++) {
    neorv32_uart0_printf("%s", tests[i].name);
    if (tests[i].ok) {
      neorv32_uart0_printf(" - OK\n");
    } else {
      neorv32_uart0_printf(" - FAIL\n");
      result = 1;
    }
  }

  // Zcmp interplay (self-skips when Zcmp is not implemented)
  neorv32_uart0_printf("cm.jalt + cm.push/cm.popret interplay");
  if (!(neorv32_cpu_csr_read(CSR_MXISAH) & (1 << CSR_MXISAH_ZCMP))) {
    neorv32_uart0_printf(" - SKIP (no Zcmp)\n");
  }
  else if (test_interplay()) {
    neorv32_uart0_printf(" - OK\n");
  }
  else {
    neorv32_uart0_printf(" - FAIL\n");
    result = 1;
  }

  neorv32_cpu_csr_write(CSR_JVT, jvt_save);

  return result;
}
