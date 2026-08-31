-- ================================================================================ --
-- NEORV32 CPU - Zcmt Table-Jump Sequencer (RISC-V 'Zcmt' ISA Extension)            --
-- -------------------------------------------------------------------------------- --
-- Implements cm.jt (index 0..31) and cm.jalt (index 32..255, links ra = pc+2).     --
-- The jump-table entry is read via the instruction fetch bus (the table access is  --
-- architecturally an instruction fetch: PMP execute permission, instruction access --
-- fault on error). Afterwards a single synthesized jalr micro-op is injected whose --
-- branch target is overridden with the fetched table entry in the control unit.    --
-- -------------------------------------------------------------------------------- --
-- The NEORV32 RISC-V Processor - https://github.com/stnolting/neorv32              --
-- Copyright (c) NEORV32 contributors.                                              --
-- Licensed under the BSD-3-Clause license, see LICENSE for details.                --
-- SPDX-License-Identifier: BSD-3-Clause                                            --
-- ================================================================================ --

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;

entity neorv32_cpu_zcmt is
  port (
    clk_i             : in  std_ulogic;
    rstn_i            : in  std_ulogic;
    ctrl_i            : in  ctrl_bus_t;
    zcmt_detect       : in  std_ulogic; -- cm.jt/cm.jalt detected by issue engine
    fetch_restart     : in  std_ulogic; -- fetch engine restart (buffered branch)
    ipb_avail         : in  std_ulogic_vector(1 downto 0);
    zcmt_instr_reg    : in  std_ulogic_vector(15 downto 0); -- latched instruction word (shared with Zcmp)
    jvt_i             : in  std_ulogic_vector(31 downto 0); -- jump-table base address (jvt CSR, bits 5:0 zero)
    -- jump-table read interface (to fetch engine) --
    tbl_req_o         : out std_ulogic; -- level: table read requested
    tbl_addr_o        : out std_ulogic_vector(31 downto 0); -- table entry address
    tbl_ack_i         : in  std_ulogic; -- one-shot: response valid
    tbl_data_i        : in  std_ulogic_vector(31 downto 0); -- table entry data
    tbl_err_i         : in  std_ulogic; -- bus/PMP error
    -- front-end bus (micro-op injection) --
    frontend_bus_zcmt : out if_bus_t;
    zcmt_in_uop_seq   : out std_ulogic;
    zcmt_target_o     : out std_ulogic_vector(31 downto 0) -- registered table entry (S_BRANCH target, bit 0 cleared)
  );
end neorv32_cpu_zcmt;

architecture neorv32_cpu_zcmt_rtl of neorv32_cpu_zcmt is

  type state_t is (S_IDLE, S_TABLE_FETCH, S_UOP, S_ZCMT_ABORT);
  signal state_reg, state_nxt : state_t;

  -- registered table-read response (bit 0 of the entry is discarded - cleared per jalr semantics) --
  signal data_r : std_ulogic_vector(31 downto 1);
  signal err_r  : std_ulogic;

  signal in_seq_int : std_ulogic;
  signal tbl_req_int : std_ulogic;

  -- synthesized micro-ops --
  constant jalr_jt_c   : std_ulogic_vector(31 downto 0) := x"00000067"; -- jalr x0, 0(x0); target overridden by zcmt_target
  constant jalr_jalt_c : std_ulogic_vector(31 downto 0) := x"000000E7"; -- jalr x1, 0(x0); target overridden by zcmt_target
  constant nop_c       : std_ulogic_vector(31 downto 0) := x"00000013"; -- nop; fault carrier (must not link)

  -- cm.jalt when index >= 32, i.e. index(7:5) = instr(9:7) /= "000" --
  signal is_link : std_ulogic;

begin

  is_link <= '0' when (zcmt_instr_reg(9 downto 7) = "000") else '1';

  -- jump-table entry address: jvt.base + zero-extended index * 4
  -- (jvt and the latched instruction word are stable for the whole sequence) --
  tbl_addr_o <= std_ulogic_vector(unsigned(jvt_i) + resize(unsigned(zcmt_instr_reg(9 downto 2)) & "00", 32));

  zcmt_in_uop_seq <= in_seq_int;
  tbl_req_o       <= tbl_req_int;

  -- registered table entry, routed past the front-end bus mux to the control unit's S_BRANCH
  -- target override. Stability: the jalr micro-op is unconditional and reaches S_BRANCH exactly
  -- two cycles after its dispatch ack; the only writer of data_r is a new table read, which
  -- requires the issue engine to return to S_ISSUE and detect another cm.jt/cm.jalt first - by
  -- then S_BRANCH has asserted if_reset, which blocks the fetch engine's table-read divert and
  -- aborts this sequencer before any bus response can arrive --
  zcmt_target_o <= data_r & '0';

  uop_fsm_sync: process(rstn_i, clk_i)
  begin
    if (rstn_i = '0') then
      state_reg <= S_IDLE;
      data_r    <= (others => '0');
      err_r     <= '0';
    elsif rising_edge(clk_i) then
      state_reg <= state_nxt;
      if (state_reg = S_TABLE_FETCH) and (tbl_ack_i = '1') then
        data_r <= tbl_data_i(31 downto 1);
        err_r  <= tbl_err_i;
      end if;
    end if;
  end process uop_fsm_sync;

  uop_fsm_comb: process(state_reg, zcmt_detect, zcmt_instr_reg, is_link, data_r, err_r,
                        tbl_ack_i, fetch_restart, ipb_avail, in_seq_int, ctrl_i)
  begin
    -- defaults --
    state_nxt   <= state_reg;
    in_seq_int  <= '0';
    tbl_req_int <= '0';
    frontend_bus_zcmt.valid <= '0';
    frontend_bus_zcmt.compr <= '1';
    frontend_bus_zcmt.fault <= '0';
    frontend_bus_zcmt.i32   <= (others => '0');
    frontend_bus_zcmt.i16   <= zcmt_instr_reg; -- original 16-bit instruction word
    frontend_bus_zcmt.zcmp_in_uop_seq  <= in_seq_int;
    frontend_bus_zcmt.zcmp_atomic_tail <= '0'; -- Zcmt is restartable - no atomic tail
    frontend_bus_zcmt.zcmp_start       <= '0';
    frontend_bus_zcmt.zcmt_branch      <= '0';
    frontend_bus_zcmt.zcmt_target      <= (others => '0'); -- unused: overridden past the mux with zcmt_target_o

    case state_reg is

      when S_IDLE =>
        -- NOTE: zcmt_instr_reg is not yet valid in the detect cycle (it registers on the same edge)
        if (zcmt_detect = '1') then
          state_nxt <= S_TABLE_FETCH;
        end if;

      when S_TABLE_FETCH => -- read jump-table entry via the fetch engine
        in_seq_int  <= '1';
        tbl_req_int <= '1';
        if (tbl_ack_i = '1') then -- response data/error registered in sync process
          state_nxt <= S_UOP;
        end if;
        -- abort on branch shadow or trap; wins over a coincident ack. A still in-flight table
        -- read is completed by the fetch engine and its late response is ignored here --
        if (fetch_restart = '1') or (ctrl_i.cpu_trap = '1') then
          state_nxt   <= S_ZCMT_ABORT;
          in_seq_int  <= '0';
          tbl_req_int <= '0';
        end if;

      when S_UOP => -- inject the single jalr micro-op (or a faulting nop)
        in_seq_int <= '1';
        frontend_bus_zcmt.valid <= '1';
        if (err_r = '1') then -- table read failed: faulting nop -> instruction access fault, mepc = address of cm.jt/cm.jalt, no link
          frontend_bus_zcmt.fault <= '1';
          frontend_bus_zcmt.i32   <= nop_c;
        else
          frontend_bus_zcmt.i32 <= jalr_jalt_c when (is_link = '1') else jalr_jt_c;
          frontend_bus_zcmt.zcmt_branch <= '1'; -- control unit overrides the S_BRANCH target (with zcmt_target_o)
        end if;
        if (ctrl_i.if_ready = '1') then -- micro-op acknowledged by control unit
          state_nxt <= S_IDLE;
        end if;
        -- abort clause last: wins over a coincident ack --
        if (fetch_restart = '1') or (ctrl_i.cpu_trap = '1') then
          state_nxt  <= S_ZCMT_ABORT;
          in_seq_int <= '0';
          frontend_bus_zcmt.valid <= '0';
          frontend_bus_zcmt.fault <= '0';
          frontend_bus_zcmt.i32   <= (others => '0');
          frontend_bus_zcmt.zcmt_branch <= '0';
        end if;

      when S_ZCMT_ABORT => -- wait for the prefetch buffer to refill after the restart
        if (ipb_avail /= "00") then
          if (zcmt_detect = '1') then -- next instruction can be another cm.jt/cm.jalt
            state_nxt <= S_TABLE_FETCH;
          else
            state_nxt <= S_IDLE;
          end if;
        end if;

    end case;
  end process uop_fsm_comb;

end neorv32_cpu_zcmt_rtl;
