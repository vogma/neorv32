#!/usr/bin/env python3

from pathlib import Path
from vunit import VUnit

ROOT = Path(__file__).resolve().parents[2]

vu = VUnit.from_argv(compile_builtins=False)
vu.add_vhdl_builtins()
lib = vu.add_library("neorv32")

# RTL sources
lib.add_source_files(ROOT / "rtl" / "core" / "*.vhd")

# VUnit testbench + helpers
lib.add_source_files(ROOT / "sim" / "vunit" / "*.vhd")

# Test matrix
tb = lib.test_bench("tb_cache")
tb.set_sim_option("ghdl.sim_flags", ["--wave=cache.ghw"])
# lib.set_sim_option("ghdl.sim_flags", ["--ieee-asserts=none-at-all"])

for num_blocks in [4, 16]:
    for block_size in [8, 32]:
        for bursts_en in [True, False]:
            cfg_name = f"NB{num_blocks}_BS{block_size}_B{'on' if bursts_en else 'off'}"
            tb.add_config(
                name=cfg_name,
                generics={
                    "NUM_BLOCKS": num_blocks,
                    "BLOCK_SIZE": block_size,
                    "BURSTS_EN": bursts_en,
                    "READ_ONLY": False,
                    "UC_BEGIN": "1111",  # 0xF
                },
            )

vu.main()
