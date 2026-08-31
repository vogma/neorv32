# Application makefile.
# Use this makefile to configure all relevant CPU / compiler options.

# Override the default CPU ISA
# NOTE: deliberately WITHOUT _zcmt so GCC never emits cm.jt/cm.jalt itself;
# the tests hand-encode the instructions via .hword
MARCH = rv32imac_zicsr_zifencei

# Override the default RISC-V GCC prefix
RISCV_PREFIX ?= riscv32-unknown-elf-

# Override default optimization goal
EFFORT = -Os

# Add extended debug symbols
USER_FLAGS += -ggdb -gdwarf-3

# Adjust processor IMEM size
USER_FLAGS += -Wl,--defsym,__neorv32_rom_size=32k

# Adjust processor DMEM size
USER_FLAGS += -Wl,--defsym,__neorv32_ram_size=8k

# The full suite needs more than ghdl.sh's default 10ms of simulation time
GHDL_RUN_FLAGS ?= --stop-time=20ms

# Set path to NEORV32 root directory
NEORV32_HOME ?= ../../..

# Include the main NEORV32 makefile
include $(NEORV32_HOME)/sw/common/common.mk

sim-check: sim
	cat $(NEORV32_HOME)/sim/tb.uart0_rx.log | grep "\[ZCMT\] ALL TESTS PASSED"
