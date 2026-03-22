# Running RISC-V Architecture Compatibility Tests

## Prerequisites

### Toolchain

- `riscv-none-elf-gcc` cross compiler on PATH

### Python packages

Install with `pip install --user`:

- `riscof==1.25.2` (1.25.3 is broken — imports a non-existent `preprocessing` from `riscv-isac`)
- `riscv-isac>=0.18.0`
- `riscv-config>=3.18.3`

### RISC-V Sail reference model

The plugin expects the binary **`sail_riscv_sim`** (not `sail`) on PATH.
This is the pre-built RISC-V simulator from [sail-riscv](https://github.com/riscv/sail-riscv),
not the Sail language compiler.

Download the RV32 Linux release from:
https://github.com/riscv/sail-riscv/releases

### GHDL

GHDL with `--std=08` support (VHDL-2008).

### Git submodule

```bash
git submodule update --init sw/riscv-arch-test/riscv-arch-test
```

## Running

```bash
sh sw/riscv-arch-test/run.sh
```

The script runs riscof, compares DUT signatures against the Sail reference model,
and exits 0 on success or 1 on failure. The HTML report is at `sw/riscv-arch-test/riscof_work/report.html`.

## Configuration

Edit `sw/riscv-arch-test/config.ini` to adjust parallelism (`jobs`) for both plugins.

DUT ISA and platform specs are in:
- `plugins/neorv32/neorv32_isa.yaml`
- `plugins/neorv32/neorv32_platform.yaml`

## Notes

- Warnings about "Test Selected without the relevant extensions being available on DUT"
  are expected — the suite includes RV64 and extension tests that get filtered out for the
  RV32 NEORV32 configuration.
- The arch tests validate ISA correctness only, not cache performance or timing.
