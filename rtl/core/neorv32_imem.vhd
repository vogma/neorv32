-- ================================================================================ --
-- NEORV32 SoC - Processor-Internal Instruction Memory (IMEM)                       --
-- -------------------------------------------------------------------------------- --
-- [TIP] This file can be replaced by a technology-specific implementation to       --
--       optimize timing, area, energy, etc.                                        --
-- -------------------------------------------------------------------------------- --
-- The NEORV32 RISC-V Processor - https://github.com/stnolting/neorv32              --
-- Copyright (c) NEORV32 contributors.                                              --
-- Copyright (c) 2020 - 2024 Stephan Nolting. All rights reserved.                  --
-- Licensed under the BSD-3-Clause license, see LICENSE for details.                --
-- SPDX-License-Identifier: BSD-3-Clause                                            --
-- ================================================================================ --

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;
use neorv32.neorv32_application_image.all; -- generated by the image generator

entity neorv32_imem is
  generic (
    IMEM_SIZE : natural; -- memory size in bytes, has to be a power of 2, min 4
    IMEM_INIT : boolean  -- implement IMEM as pre-initialized read-only memory?
  );
  port (
    clk_i     : in  std_ulogic; -- global clock line
    rstn_i    : in  std_ulogic; -- async reset, low-active
    bus_req_i : in  bus_req_t;  -- bus request
    bus_rsp_o : out bus_rsp_t   -- bus response
  );
end neorv32_imem;

architecture neorv32_imem_rtl of neorv32_imem is

  -- alternative memory description style --
  constant alt_style_c : boolean := false; -- [TIP] enable this if synthesis fails to infer block RAM

  -- ROM - initialized with executable code --
  constant imem_app_size_c : natural := (application_init_image'length)*4; -- application (image) size in bytes
  constant mem_rom_c : mem32_t(0 to IMEM_SIZE/4-1) := mem32_init_f(application_init_image, IMEM_SIZE/4);

  -- local signals --
  signal rdata         : std_ulogic_vector(31 downto 0);
  signal rden          : std_ulogic;
  signal addr, addr_ff : unsigned(index_size_f(IMEM_SIZE/4)-1 downto 0);

  -- [NOTE] The memory (RAM) is built from 4 individual byte-wide memories as some synthesis tools
  --        have issues inferring 32-bit memories with individual byte-enable signals.
  -- [NOTE] Read-during-write behavior is irrelevant
  --        as read and write accesses are mutually exclusive (ensured by bus protocol).
  signal mem_ram_b0, mem_ram_b1, mem_ram_b2, mem_ram_b3 : mem8_t(0 to IMEM_SIZE/4-1);

begin

  -- Sanity Checks --------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  assert false report
    "[NEORV32] Implementing processor-internal IMEM as " &
    cond_sel_string_f(IMEM_INIT, "pre-initialized ROM.", "blank RAM.") severity note;

  assert not ((IMEM_INIT = true) and (imem_app_size_c > IMEM_SIZE)) report
    "[NEORV32] Application image (" & natural'image(imem_app_size_c) &
    " bytes) does not fit into processor-internal IMEM (" &
    natural'image(IMEM_SIZE) & " bytes)!" severity error;


  -- Implement IMEM as pre-initialized ROM --------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  imem_rom:
  if IMEM_INIT generate

    imem_rom_default: -- default memory HDL style
    if not alt_style_c generate
      mem_access: process(clk_i)
      begin
        if rising_edge(clk_i) then
          rdata <= mem_rom_c(to_integer(addr));
        end if;
      end process mem_access;
      addr_ff <= (others => '0'); -- unused
    end generate;

    imem_rom_alternative: -- alternative memory HDL style
    if alt_style_c generate
      mem_access: process(clk_i)
      begin
        if rising_edge(clk_i) then
          addr_ff <= addr;
        end if;
      end process mem_access;
      rdata <= mem_rom_c(to_integer(addr_ff));
    end generate;

  end generate;

  -- word aligned access address --
  addr <= unsigned(bus_req_i.addr(index_size_f(IMEM_SIZE/4)+1 downto 2));


  -- Implement IMEM as non-initialized RAM --------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  imem_ram:
  if not IMEM_INIT generate

    imem_ram_default: -- default memory HDL style
    if not alt_style_c generate
      mem_access: process(clk_i)
      begin
        if rising_edge(clk_i) then
          if (bus_req_i.stb = '1') and (bus_req_i.rw = '1') then
            if (bus_req_i.ben(0) = '1') then -- byte 0
              mem_ram_b0(to_integer(addr)) <= bus_req_i.data(7 downto 0);
            end if;
            if (bus_req_i.ben(1) = '1') then -- byte 1
              mem_ram_b1(to_integer(addr)) <= bus_req_i.data(15 downto 8);
            end if;
            if (bus_req_i.ben(2) = '1') then -- byte 2
              mem_ram_b2(to_integer(addr)) <= bus_req_i.data(23 downto 16);
            end if;
            if (bus_req_i.ben(3) = '1') then -- byte 3
              mem_ram_b3(to_integer(addr)) <= bus_req_i.data(31 downto 24);
            end if;
          end if;
          rdata(7  downto 0)  <= mem_ram_b0(to_integer(addr));
          rdata(15 downto 8)  <= mem_ram_b1(to_integer(addr));
          rdata(23 downto 16) <= mem_ram_b2(to_integer(addr));
          rdata(31 downto 24) <= mem_ram_b3(to_integer(addr));
        end if;
      end process mem_access;
      addr_ff <= (others => '0'); -- unused
    end generate;

    imem_ram_alternative: -- alternative memory HDL style
    if alt_style_c generate
      mem_access: process(clk_i)
      begin
        if rising_edge(clk_i) then
          addr_ff <= addr;
          if (bus_req_i.stb = '1') and (bus_req_i.rw = '1') then
            if (bus_req_i.ben(0) = '1') then -- byte 0
              mem_ram_b0(to_integer(addr)) <= bus_req_i.data(7 downto 0);
            end if;
            if (bus_req_i.ben(1) = '1') then -- byte 1
              mem_ram_b1(to_integer(addr)) <= bus_req_i.data(15 downto 8);
            end if;
            if (bus_req_i.ben(2) = '1') then -- byte 2
              mem_ram_b2(to_integer(addr)) <= bus_req_i.data(23 downto 16);
            end if;
            if (bus_req_i.ben(3) = '1') then -- byte 3
              mem_ram_b3(to_integer(addr)) <= bus_req_i.data(31 downto 24);
            end if;
          end if;
        end if;
      end process mem_access;
      rdata(7  downto 0)  <= mem_ram_b0(to_integer(addr_ff));
      rdata(15 downto 8)  <= mem_ram_b1(to_integer(addr_ff));
      rdata(23 downto 16) <= mem_ram_b2(to_integer(addr_ff));
      rdata(31 downto 24) <= mem_ram_b3(to_integer(addr_ff));
    end generate;

  end generate;


  -- Bus Response ---------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  bus_feedback: process(rstn_i, clk_i)
  begin
    if (rstn_i = '0') then
      rden          <= '0';
      bus_rsp_o.ack <= '0';
    elsif rising_edge(clk_i) then
      rden <= bus_req_i.stb and (not bus_req_i.rw);
      if (IMEM_INIT = true) then
        bus_rsp_o.ack <= bus_req_i.stb and (not bus_req_i.rw); -- read-only!
      else
        bus_rsp_o.ack <= bus_req_i.stb;
      end if;
    end if;
  end process bus_feedback;

  bus_rsp_o.data <= rdata when (rden = '1') else (others => '0'); -- output gate
  bus_rsp_o.err  <= '0'; -- no access error possible


end neorv32_imem_rtl;
