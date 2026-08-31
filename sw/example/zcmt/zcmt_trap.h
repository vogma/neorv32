#pragma once

#include <neorv32.h>

// shared minimal trap-recording machinery for the Zcmt fault tests:
// records mepc/mcause/mtval/ra of the trapped context, increments a counter
// and redirects execution to zt_resume (mepc is NOT advanced - the faulting
// instruction would fault again).

extern volatile uint32_t zt_mepc;
extern volatile uint32_t zt_mcause;
extern volatile uint32_t zt_mtval;
extern volatile uint32_t zt_ra;    // x1 of the trapped context
extern volatile uint32_t zt_count;
extern volatile uint32_t zt_resume;

void zt_install_handler(void);
void zt_restore_handler(void);
void zt_reset(void);
