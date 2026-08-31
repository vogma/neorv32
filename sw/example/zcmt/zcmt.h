#pragma once

#include <neorv32.h>

// cm.jt / cm.jalt encoding: C2 quadrant (bits 1:0 = 10), funct3 = 101,
// instr[12:10] = 000, index = instr[9:2].
// cm.jt:   index 0..31   (no register written)
// cm.jalt: index 32..255 (writes ra = pc+2)
#define ZCMT_ENC(idx) (0xA002u + ((idx) << 2))

// shared jump targets and result globals (defined in cm_jt.c)
void zcmt_target_mark1(void); // sets zcmt_hit_marker = 0xA1, ret
void zcmt_target_mark2(void); // sets zcmt_hit_marker = 0xB2, ret
void zcmt_target_link(void);  // captures ra, sets zcmt_hit_marker = 0xC3, ret
extern volatile uint32_t zcmt_hit_marker;
extern volatile uint32_t zcmt_captured_ra;

int cm_jvt(void);
int cm_jvt_disabled(void);
int cm_jt(void);
int cm_tbl_fault(void);
int cm_tgt_fault(void);
int cm_irq(void);
int cm_race(void);
int cm_umode(void);
int cm_pmp_fault(void);
