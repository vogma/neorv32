// ================================================================================ //
// The NEORV32 RISC-V Processor - https://github.com/stnolting/neorv32              //
// Copyright (c) NEORV32 contributors.                                              //
// Copyright (c) 2020 - 2024 Stephan Nolting. All rights reserved.                  //
// Licensed under the BSD-3-Clause license, see LICENSE for details.                //
// SPDX-License-Identifier: BSD-3-Clause                                            //
// ================================================================================ //

/**
 * @file neorv32_cpu.h
 * @brief CPU Core Functions HW driver header file.
 *
 * @see https://stnolting.github.io/neorv32/sw/files.html
 */

#ifndef neorv32_cpu_h
#define neorv32_cpu_h

#include <stdint.h>


/**********************************************************************//**
 * @name Prototypes
 **************************************************************************/
/**@{*/
uint64_t neorv32_cpu_get_cycle(void);
void     neorv32_cpu_set_mcycle(uint64_t value);
uint64_t neorv32_cpu_get_instret(void);
void     neorv32_cpu_set_minstret(uint64_t value);
void     neorv32_cpu_delay_ms(uint32_t time_ms);
uint32_t neorv32_cpu_get_clk_from_prsc(int prsc);
uint32_t neorv32_cpu_pmp_get_num_regions(void);
uint32_t neorv32_cpu_pmp_get_granularity(void);
int      neorv32_cpu_pmp_configure_region(int index, uint32_t addr, uint8_t config);
uint32_t neorv32_cpu_hpm_get_num_counters(void);
uint32_t neorv32_cpu_hpm_get_size(void);
void     neorv32_cpu_goto_user_mode(void);
/**@}*/


// #################################################################################################
// Context save/restore helpers
// #################################################################################################


/**********************************************************************//**
 * Save all integer registers to the stack.
 *
 * @note This inlined function automatically constrains the number
 * of registers when compiling for rv32e (only 16 registers).
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_context_save(void) {

  // do not backup x0 and sp
  asm volatile (
#ifndef __riscv_32e
    "addi sp, sp, -30*4 \n"
#else
    "addi sp, sp, -14*4 \n"
#endif
    "sw x1,   0*4(sp) \n"
    "sw x3,   1*4(sp) \n"
    "sw x4,   2*4(sp) \n"
    "sw x5,   3*4(sp) \n"
    "sw x6,   4*4(sp) \n"
    "sw x7,   5*4(sp) \n"
    "sw x8,   6*4(sp) \n"
    "sw x9,   7*4(sp) \n"
    "sw x10,  8*4(sp) \n"
    "sw x11,  9*4(sp) \n"
    "sw x12, 10*4(sp) \n"
    "sw x13, 11*4(sp) \n"
    "sw x14, 12*4(sp) \n"
    "sw x15, 13*4(sp) \n"
#ifndef __riscv_32e
    "sw x16, 14*4(sp) \n"
    "sw x17, 15*4(sp) \n"
    "sw x18, 16*4(sp) \n"
    "sw x19, 17*4(sp) \n"
    "sw x20, 18*4(sp) \n"
    "sw x21, 19*4(sp) \n"
    "sw x22, 20*4(sp) \n"
    "sw x23, 21*4(sp) \n"
    "sw x24, 22*4(sp) \n"
    "sw x25, 23*4(sp) \n"
    "sw x26, 24*4(sp) \n"
    "sw x27, 25*4(sp) \n"
    "sw x28, 26*4(sp) \n"
    "sw x29, 27*4(sp) \n"
    "sw x30, 28*4(sp) \n"
    "sw x31, 29*4(sp) \n"
#endif
  );
}


/**********************************************************************//**
 * Restore all integer registers from the stack.
 *
 * @note This inlined function automatically constrains the number
 * of registers when compiling for rv32e (only 16 registers).
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_context_restore(void) {

  // do not restore x0 and sp
  asm volatile (
    "lw x1,   0*4(sp) \n"
    "lw x3,   1*4(sp) \n"
    "lw x4,   2*4(sp) \n"
    "lw x5,   3*4(sp) \n"
    "lw x6,   4*4(sp) \n"
    "lw x7,   5*4(sp) \n"
    "lw x8,   6*4(sp) \n"
    "lw x9,   7*4(sp) \n"
    "lw x10,  8*4(sp) \n"
    "lw x11,  9*4(sp) \n"
    "lw x12, 10*4(sp) \n"
    "lw x13, 11*4(sp) \n"
    "lw x14, 12*4(sp) \n"
    "lw x15, 13*4(sp) \n"
#ifndef __riscv_32e
    "lw x16, 14*4(sp) \n"
    "lw x17, 15*4(sp) \n"
    "lw x18, 16*4(sp) \n"
    "lw x19, 17*4(sp) \n"
    "lw x20, 18*4(sp) \n"
    "lw x21, 19*4(sp) \n"
    "lw x22, 20*4(sp) \n"
    "lw x23, 21*4(sp) \n"
    "lw x24, 22*4(sp) \n"
    "lw x25, 23*4(sp) \n"
    "lw x26, 24*4(sp) \n"
    "lw x27, 25*4(sp) \n"
    "lw x28, 26*4(sp) \n"
    "lw x29, 27*4(sp) \n"
    "lw x30, 28*4(sp) \n"
    "lw x31, 29*4(sp) \n"
#endif
#ifndef __riscv_32e
    "addi sp, sp, +30*4 \n"
#else
    "addi sp, sp, +14*4 \n"
#endif
    "ret              \n"
  );
}


// #################################################################################################
// Load/store helpers
// #################################################################################################


/**********************************************************************//**
 * Store unsigned word to address space.
 *
 * @note An unaligned access address will raise an alignment exception.
 *
 * @param[in] addr Address (32-bit).
 * @param[in] wdata Data word (32-bit) to store.
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_store_unsigned_word(uint32_t addr, uint32_t wdata) {

  uint32_t reg_addr = addr;
  uint32_t reg_data = wdata;

  asm volatile ("sw %[da], 0(%[ad])" : : [da] "r" (reg_data), [ad] "r" (reg_addr));
}


/**********************************************************************//**
 * Store unsigned half-word to address space.
 *
 * @note An unaligned access address will raise an alignment exception.
 *
 * @param[in] addr Address (32-bit).
 * @param[in] wdata Data half-word (16-bit) to store.
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_store_unsigned_half(uint32_t addr, uint16_t wdata) {

  uint32_t reg_addr = addr;
  uint32_t reg_data = (uint32_t)wdata;

  asm volatile ("sh %[da], 0(%[ad])" : : [da] "r" (reg_data), [ad] "r" (reg_addr));
}


/**********************************************************************//**
 * Store unsigned byte to address space.
 *
 * @param[in] addr Address (32-bit).
 * @param[in] wdata Data byte (8-bit) to store.
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_store_unsigned_byte(uint32_t addr, uint8_t wdata) {

  uint32_t reg_addr = addr;
  uint32_t reg_data = (uint32_t)wdata;

  asm volatile ("sb %[da], 0(%[ad])" : : [da] "r" (reg_data), [ad] "r" (reg_addr));
}


/**********************************************************************//**
 * Load unsigned word from address space.
 *
 * @note An unaligned access address will raise an alignment exception.
 *
 * @param[in] addr Address (32-bit).
 * @return Read data word (32-bit).
 **************************************************************************/
inline uint32_t __attribute__ ((always_inline)) neorv32_cpu_load_unsigned_word(uint32_t addr) {

  uint32_t reg_addr = addr;
  uint32_t reg_data;

  asm volatile ("lw %[da], 0(%[ad])" : [da] "=r" (reg_data) : [ad] "r" (reg_addr));

  return reg_data;
}


/**********************************************************************//**
 * Load unsigned half-word from address space.
 *
 * @note An unaligned access address will raise an alignment exception.
 *
 * @param[in] addr Address (32-bit).
 * @return Read data half-word (16-bit).
 **************************************************************************/
inline uint16_t __attribute__ ((always_inline)) neorv32_cpu_load_unsigned_half(uint32_t addr) {

  uint32_t reg_addr = addr;
  uint16_t reg_data;

  asm volatile ("lhu %[da], 0(%[ad])" : [da] "=r" (reg_data) : [ad] "r" (reg_addr));

  return reg_data;
}


/**********************************************************************//**
 * Load signed half-word from address space.
 *
 * @note An unaligned access address will raise an alignment exception.
 *
 * @param[in] addr Address (32-bit).
 * @return Read data half-word (16-bit).
 **************************************************************************/
inline int16_t __attribute__ ((always_inline)) neorv32_cpu_load_signed_half(uint32_t addr) {

  uint32_t reg_addr = addr;
  int16_t reg_data;

  asm volatile ("lh %[da], 0(%[ad])" : [da] "=r" (reg_data) : [ad] "r" (reg_addr));

  return reg_data;
}


/**********************************************************************//**
 * Load unsigned byte from address space.
 *
 * @param[in] addr Address (32-bit).
 * @return Read data byte (8-bit).
 **************************************************************************/
inline uint8_t __attribute__ ((always_inline)) neorv32_cpu_load_unsigned_byte(uint32_t addr) {

  uint32_t reg_addr = addr;
  uint8_t reg_data;

  asm volatile ("lbu %[da], 0(%[ad])" : [da] "=r" (reg_data) : [ad] "r" (reg_addr));

  return reg_data;
}


/**********************************************************************//**
 * Load signed byte from address space.
 *
 * @param[in] addr Address (32-bit).
 * @return Read data byte (8-bit).
 **************************************************************************/
inline int8_t __attribute__ ((always_inline)) neorv32_cpu_load_signed_byte(uint32_t addr) {

  uint32_t reg_addr = addr;
  int8_t reg_data;

  asm volatile ("lb %[da], 0(%[ad])" : [da] "=r" (reg_data) : [ad] "r" (reg_addr));

  return reg_data;
}


// #################################################################################################
// CSR access helpers
// #################################################################################################


/**********************************************************************//**
 * Read data from CPU control and status register (CSR).
 *
 * @param[in] csr_id ID of CSR to read. See #NEORV32_CSR_enum.
 * @return Read data (uint32_t).
 **************************************************************************/
inline uint32_t __attribute__ ((always_inline)) neorv32_cpu_csr_read(const int csr_id) {

  uint32_t csr_data;

  asm volatile ("csrr %[result], %[input_i]" : [result] "=r" (csr_data) : [input_i] "i" (csr_id));

  return csr_data;
}


/**********************************************************************//**
 * Write data to CPU control and status register (CSR).
 *
 * @param[in] csr_id ID of CSR to write. See #NEORV32_CSR_enum.
 * @param[in] data Data to write (uint32_t).
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_csr_write(const int csr_id, uint32_t data) {

  uint32_t csr_data = data;

  asm volatile ("csrw %[input_i], %[input_j]" :  : [input_i] "i" (csr_id), [input_j] "r" (csr_data));
}


/**********************************************************************//**
 * Set bit(s) in CPU control and status register (CSR).
 *
 * @param[in] csr_id ID of CSR to write. See #NEORV32_CSR_enum.
 * @param[in] mask Bit mask (high-active) to set bits (uint32_t).
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_csr_set(const int csr_id, uint32_t mask) {

  uint32_t csr_data = mask;

  asm volatile ("csrs %[input_i], %[input_j]" :  : [input_i] "i" (csr_id), [input_j] "r" (csr_data));
}


/**********************************************************************//**
 * Clear bit(s) in CPU control and status register (CSR).
 *
 * @param[in] csr_id ID of CSR to write. See #NEORV32_CSR_enum.
 * @param[in] mask Bit mask (high-active) to clear bits (uint32_t).
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_csr_clr(const int csr_id, uint32_t mask) {

  uint32_t csr_data = mask;

  asm volatile ("csrc %[input_i], %[input_j]" :  : [input_i] "i" (csr_id), [input_j] "r" (csr_data));
}


// #################################################################################################
// Misc helpers
// #################################################################################################


/**********************************************************************//**
 * Put CPU into sleep / power-down mode.
 *
 * @note The WFI (wait for interrupt) instruction will make the CPU halt until
 * any enabled interrupt source becomes pending.
 **************************************************************************/
inline void __attribute__ ((always_inline)) neorv32_cpu_sleep(void) {

  asm volatile ("wfi");
}


#endif // neorv32_cpu_h
