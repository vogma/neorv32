#!/usr/bin/env python3

import subprocess
from pathlib import Path
from vunit import VUnit

ROOT = Path(__file__).resolve().parents[2]
SIM_DIR = Path(__file__).parent

vu = VUnit.from_argv(compile_builtins=False)
vu.add_vhdl_builtins()


def ghdl_std_flag():
    """
    Select the newest supported VHDL standard for this GHDL installation.
    Ubuntu's ghdl-mcode package often ships only up to v08 precompiled libs.
    """
    try:
        result = subprocess.run(
            ["ghdl", "--dispconfig"],
            check=False,
            capture_output=True,
            text=True,
        )
        library_prefix = None
        for line in result.stdout.splitlines():
            if line.startswith("library prefix:"):
                library_prefix = line.split(":", 1)[1].strip()
                break
        if library_prefix:
            lib_path = Path(library_prefix)
            if (lib_path / "ieee" / "v19").exists() and (
                lib_path / "std" / "v19"
            ).exists():
                return "--std=19"
    except Exception:
        pass
    return "--std=08"


selected_std = ghdl_std_flag()

# GHDL compile options for OSVVM and DUT.
ghdl_flags = [selected_std, "-frelaxed-rules", "-fsynopsys"]

# ============================================================================
# OSVVM Libraries
# ============================================================================
osvvm_path = SIM_DIR.parent / "osvvm"

if not (osvvm_path / "osvvm").exists():
    print("=" * 60)
    print("OSVVM not found. Please run:")
    print(f"  cd {SIM_DIR.parent}")
    print(
        "  git submodule add https://github.com/OSVVM/OsvvmLibraries.git sim/osvvm"
    )
    print("  cd sim/osvvm && git submodule update --init --recursive")
    print("=" * 60)
    exit(1)

# OSVVM core library - exclude vendor-specific files (use default only)
osvvm_lib = vu.add_library("osvvm")
osvvm_core_path = osvvm_path / "osvvm"

# For tools limited to VHDL-2008, OSVVM provides compatibility shims.
# Auto-detect all *_c.vhd files in deprecated/ that replace a main source file.
compat_replacements = {}
if selected_std != "--std=19":
    for compat_file in sorted((osvvm_core_path / "deprecated").glob("*_c.vhd")):
        main_name = compat_file.name.replace("_c.vhd", ".vhd")
        if (osvvm_core_path / main_name).exists():
            compat_replacements[main_name] = compat_file

for f in sorted(osvvm_core_path.glob("*.vhd")):
    # Skip vendor-specific implementations (Aldec, NVC, etc.) - use default
    if "_Aldec" in f.name or "_NVC" in f.name or "_Mentor" in f.name:
        continue
    osvvm_lib.add_source_files(compat_replacements.get(f.name, f))

# OSVVM Common library (required by AXI4)
osvvm_common_lib = vu.add_library("osvvm_common")
osvvm_common_lib.add_source_files(osvvm_path / "Common" / "src" / "*.vhd")

# OSVVM AXI4 library
osvvm_axi4_lib = vu.add_library("osvvm_axi4")
osvvm_axi4_lib.add_source_files(osvvm_path / "AXI4" / "common" / "src" / "*.vhd")
osvvm_axi4_lib.add_source_files(osvvm_path / "AXI4" / "Axi4" / "src" / "*.vhd")

# ============================================================================
# NEORV32 Library
# ============================================================================
lib = vu.add_library("neorv32")

# RTL sources
lib.add_source_files(ROOT / "rtl" / "core" / "*.vhd")

# AXI bridge (needed by tb_cache_chain)
lib.add_source_files(ROOT / "rtl" / "system_integration" / "xbus2axi4_bridge.vhd")

# VUnit testbenches + helpers
lib.add_source_files(ROOT / "sim" / "vunit" / "*.vhd")

# ============================================================================
# Compile and Simulation Options (must be set AFTER adding sources)
# ============================================================================
for l in [osvvm_lib, osvvm_common_lib, osvvm_axi4_lib, lib]:
    l.set_compile_option("ghdl.a_flags", ghdl_flags)

vu.set_sim_option("ghdl.elab_flags", ghdl_flags + ["--syn-binding"])

# ============================================================================
# Existing tb_cache configurations (8-config matrix)
# ============================================================================
tb = lib.test_bench("tb_cache")
tb.set_sim_option("ghdl.sim_flags", ["--wave=cache.ghw"])

for num_blocks in [4, 256]:
    for block_size in [8, 64]:
        for bursts_en in [True, False]:
            cfg_name = (
                f"NB{num_blocks}_BS{block_size}_B{'on' if bursts_en else 'off'}"
            )
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

# ============================================================================
# New tb_cache_chain configurations (4-config burst × regstage matrix)
# ============================================================================
tb_chain = lib.test_bench("tb_cache_chain")
tb_chain.set_sim_option("ghdl.sim_flags", ["--wave=cache_chain.ghw"])

for bursts in [True, False]:
    for regstage in [False, True]:
        name = f"burst_{'on' if bursts else 'off'}_reg_{'on' if regstage else 'off'}"
        tb_chain.add_config(
            name=name,
            generics={"BURSTS_EN": bursts, "REGSTAGE_EN": regstage},
        )

vu.main()
