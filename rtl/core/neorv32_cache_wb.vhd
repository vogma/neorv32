-- ================================================================================ --
-- NEORV32 SoC - Generic Cache                                                      --
-- -------------------------------------------------------------------------------- --
-- Configurable generic cache module. The cache is direct-mapped and implements     --
-- "write-through" write strategy. Locked bursts operations are used if BURSTS_EN   --
-- is true; otherwise block transfers are split into single-transfers.              --
--                                                                                  --
-- Uncached / direct accesses: Several bus transaction types will bypass the cache: --
-- * atomic memory operations                                                       --
-- * accesses to the explicit "uncached address space page" (or higher),            --
--   which is defined by the 4 most significant address bits (UC_BEGIN)             --
-- -------------------------------------------------------------------------------- --
-- The NEORV32 RISC-V Processor - https://github.com/stnolting/neorv32              --
-- Copyright (c) NEORV32 contributors.                                              --
-- Copyright (c) 2020 - 2026 Stephan Nolting. All rights reserved.                  --
-- Licensed under the BSD-3-Clause license, see LICENSE for details.                --
-- SPDX-License-Identifier: BSD-3-Clause                                            --
-- ================================================================================ --

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY neorv32;
USE neorv32.neorv32_package.ALL;

ENTITY neorv32_cache_wb IS
  GENERIC (
    NUM_BLOCKS : NATURAL RANGE 1 TO 1024; -- number of cache blocks, has to be a power of 2
    BLOCK_SIZE : NATURAL RANGE 4 TO 32768; -- cache block size in bytes, has to be a power of 2
    UC_BEGIN : STD_ULOGIC_VECTOR(3 DOWNTO 0); -- begin of uncached address space (4 MSBs of address)
    READ_ONLY : BOOLEAN; -- read-only accesses for host
    BURSTS_EN : BOOLEAN -- enable issuing of burst transfers
  );
  PORT (
    clk_i : IN STD_ULOGIC; -- global clock, rising edge
    rstn_i : IN STD_ULOGIC; -- global reset, low-active, async
    host_req_i : IN bus_req_t; -- host request
    host_rsp_o : OUT bus_rsp_t; -- host response
    bus_req_o : OUT bus_req_t; -- bus request
    bus_rsp_i : IN bus_rsp_t -- bus response
  );
END neorv32_cache_wb;

ARCHITECTURE neorv32_cache_rtl OF neorv32_cache_wb IS

  -- cache data & tag RAM wrapper --
  -- [NOTE] We use component instantiation here to allow easy black-box instantiation for
  -- late component binding (e.g. when using the VHDL-to-Verilog flow with Verilog memory IP).
  COMPONENT neorv32_cache_ram
    GENERIC (
      TAG_WIDTH : NATURAL;
      IDX_WIDTH : NATURAL;
      OFS_WIDTH : NATURAL
    );
    PORT (
      clk_i : IN STD_ULOGIC;
      addr_i : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
      tag_we_i : IN STD_ULOGIC;
      tag_o : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0);
      data_we_i : IN STD_ULOGIC_VECTOR(3 DOWNTO 0);
      data_i : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
      data_o : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0)
    );
  END COMPONENT;

  -- only emit bursts if enabled and if block size is at least 8 bytes --
  CONSTANT bursts_en_c : BOOLEAN := BURSTS_EN AND BOOLEAN(BLOCK_SIZE >= 8);

  -- cache layout --
  CONSTANT block_num_c : NATURAL := 2 ** index_size_f(NUM_BLOCKS); -- extend if not a power of two
  CONSTANT block_size_c : NATURAL := 2 ** index_size_f(BLOCK_SIZE); -- extend if not a power of two
  CONSTANT offset_width_c : NATURAL := index_size_f(block_size_c/4); -- word offset
  CONSTANT index_width_c : NATURAL := index_size_f(block_num_c);
  CONSTANT tag_width_c : NATURAL := 32 - (offset_width_c + index_width_c + 2);

  -- control -> cache interface --
  TYPE cache_o_t IS RECORD
    cmd_clr : STD_ULOGIC;
    cmd_new : STD_ULOGIC;
    cmd_dirty_set : STD_ULOGIC;
    cmd_dirty_clr : STD_ULOGIC;
    addr : STD_ULOGIC_VECTOR(31 DOWNTO 0);
    data : STD_ULOGIC_VECTOR(31 DOWNTO 0);
    we : STD_ULOGIC_VECTOR(3 DOWNTO 0);
  END RECORD;
  SIGNAL cache_o : cache_o_t;

  -- cache -> control interface --
  TYPE cache_i_t IS RECORD
    hit : STD_ULOGIC;
    data : STD_ULOGIC_VECTOR(31 DOWNTO 0);
  END RECORD;
  SIGNAL cache_i : cache_i_t;

  -- cache bypass response buffer --
  SIGNAL bp_rsp : bus_rsp_t;

  -- control arbiter --
  TYPE state_t IS (
    S_IDLE, S_CHECK, S_DIRECT_REQ, S_DIRECT_RSP, S_CLEAR, S_DOWNLOAD_START, S_DOWNLOAD_WAIT, S_DOWNLOAD_RUN, S_DONE, S_DELAY
  );
  TYPE ctrl_t IS RECORD
    state : state_t; -- state machine
    bus_err : STD_ULOGIC; -- access error
    buf_req : STD_ULOGIC; -- access request buffer
    buf_syn : STD_ULOGIC; -- synchronization request buffer
    buf_dir : STD_ULOGIC; -- direct/uncached access buffer
    tag_idx : STD_ULOGIC_VECTOR((tag_width_c + index_width_c) - 1 DOWNTO 0); -- tag & index
    ofs_int : STD_ULOGIC_VECTOR(offset_width_c - 1 DOWNTO 0); -- cache address offset
    ofs_ext : STD_ULOGIC_VECTOR(offset_width_c DOWNTO 0); -- bus address offset
    wb_tag : STD_ULOGIC_VECTOR(tag_width_c - 1 DOWNTO 0);
    flush_idx : STD_ULOGIC_VECTOR(index_width_c - 1 DOWNTO 0);
    wb_flush : STD_ULOGIC;
  END RECORD;
  SIGNAL ctrl, ctrl_nxt : ctrl_t;

  -- status memory --
  SIGNAL valid : STD_ULOGIC_VECTOR(NUM_BLOCKS - 1 DOWNTO 0);
  SIGNAL valid_rd : STD_ULOGIC;
  SIGNAL tag_reg : STD_ULOGIC_VECTOR(tag_width_c - 1 DOWNTO 0);
  SIGNAL tag_rd : STD_ULOGIC_VECTOR(31 DOWNTO 0);

BEGIN

  -- Control Engine FSM Sync ----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  ctrl_engine_sync : PROCESS (rstn_i, clk_i)
  BEGIN
    IF (rstn_i = '0') THEN
      ctrl.state <= S_IDLE;
      ctrl.bus_err <= '0';
      ctrl.buf_req <= '0';
      ctrl.buf_syn <= '0';
      ctrl.buf_dir <= '0';
      ctrl.tag_idx <= (OTHERS => '0');
      ctrl.ofs_int <= (OTHERS => '0');
      ctrl.ofs_ext <= (OTHERS => '0');
      ctrl.wb_tag <= (OTHERS => '0');
      ctrl.flush_idx <= (OTHERS => '0');
      ctrl.wb_flush <= '0';
    ELSIF rising_edge(clk_i) THEN
      ctrl <= ctrl_nxt;
    END IF;
  END PROCESS ctrl_engine_sync;
  -- Control Engine FSM Comb ----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  ctrl_engine_comb : PROCESS (ctrl, host_req_i, cache_i, bus_rsp_i, bp_rsp)
  BEGIN
    -- control engine defaults --
    ctrl_nxt.state <= ctrl.state;
    ctrl_nxt.bus_err <= ctrl.bus_err;
    ctrl_nxt.buf_req <= ctrl.buf_req OR host_req_i.stb;
    ctrl_nxt.buf_syn <= ctrl.buf_syn OR host_req_i.fence;
    ctrl_nxt.buf_dir <= '0';
    ctrl_nxt.tag_idx <= ctrl.tag_idx;
    ctrl_nxt.ofs_int <= ctrl.ofs_int;
    ctrl_nxt.ofs_ext <= ctrl.ofs_ext;
    ctrl_nxt.wb_tag <= ctrl.wb_tag;
    ctrl_nxt.flush_idx <= ctrl.flush_idx;
    ctrl_nxt.wb_flush <= ctrl.wb_flush;

    -- cache access defaults --
    cache_o.cmd_clr <= '0';
    cache_o.cmd_new <= '0';
    cache_o.cmd_dirty_set <= '0';
    cache_o.cmd_dirty_clr <= '0';
    cache_o.addr <= host_req_i.addr;
    cache_o.we <= (OTHERS => '0');
    cache_o.data <= host_req_i.data;

    -- host response defaults --
    host_rsp_o <= rsp_terminate_c; -- default: all off
    host_rsp_o.data <= cache_i.data; -- cache read data (for cache hit)

    -- bus interface defaults --
    bus_req_o <= req_terminate_c; -- default: all off
    bus_req_o.meta <= host_req_i.meta;
    bus_req_o.addr <= ctrl.tag_idx & ctrl.ofs_ext(offset_width_c - 1 DOWNTO 0) & "00"; -- cache access
    bus_req_o.ben <= (OTHERS => '1'); -- cache updates use full-word accesses only

    -- fsm --
    CASE ctrl.state IS

      WHEN S_IDLE => -- wait for request
        -- ------------------------------------------------------------
        ctrl_nxt.bus_err <= '0'; -- reset bus error flag
        IF (ctrl.buf_syn = '1') THEN -- pending sync request
          ctrl_nxt.state <= S_CLEAR;
        ELSIF (host_req_i.stb = '1') OR (ctrl.buf_req = '1') THEN -- (pending) access request
          IF (unsigned(host_req_i.addr(31 DOWNTO 28)) >= unsigned(UC_BEGIN)) OR (host_req_i.amo = '1') THEN
            ctrl_nxt.buf_dir <= '1'; -- uncached address space access / atomic operation
          END IF;
          ctrl_nxt.state <= S_CHECK;
        END IF;

      WHEN S_CHECK => -- check access request
        -- ------------------------------------------------------------
        ctrl_nxt.tag_idx <= host_req_i.addr(31 DOWNTO 32 - (tag_width_c + index_width_c));
        ctrl_nxt.ofs_ext <= (OTHERS => '0');
        ctrl_nxt.ofs_int <= (OTHERS => '0');
        ctrl_nxt.buf_req <= '0'; -- access about to be completed
        --
        IF (ctrl.buf_dir = '1') THEN -- direct/uncached access; no cache update
          ctrl_nxt.state <= S_DIRECT_REQ;
        ELSIF (cache_i.hit = '1') THEN -- cache HIT
          IF (host_req_i.rw = '0') OR READ_ONLY THEN -- read from cache
            host_rsp_o.ack <= '1';
            ctrl_nxt.state <= S_IDLE;
          ELSE -- write to main memory and also to the cache
            cache_o.we <= host_req_i.ben;
            ctrl_nxt.state <= S_DIRECT_REQ; -- write-through
          END IF;
        ELSE -- cache MISS
          IF (host_req_i.rw = '0') OR READ_ONLY THEN -- read miss
            ctrl_nxt.state <= S_DOWNLOAD_START; -- get block from main memory
          ELSE -- write miss
            ctrl_nxt.state <= S_DIRECT_REQ; -- write-through
          END IF;
        END IF;

      WHEN S_DIRECT_REQ => -- direct memory access request
        -- ------------------------------------------------------------
        bus_req_o <= host_req_i;
        bus_req_o.stb <= '1';
        ctrl_nxt.state <= S_DIRECT_RSP;

      WHEN S_DIRECT_RSP => -- direct memory access response
        -- ------------------------------------------------------------
        bus_req_o <= host_req_i;
        bus_req_o.stb <= '0'; -- to prevent pass-through of STB signal
        host_rsp_o <= bp_rsp; -- registered response
        IF (bp_rsp.ack = '1') THEN
          ctrl_nxt.state <= S_IDLE;
        END IF;

      WHEN S_CLEAR => -- invalidate entire cache
        -- ------------------------------------------------------------
        bus_req_o.fence <= bool_to_ulogic_f(NOT READ_ONLY);
        cache_o.cmd_clr <= '1';
        ctrl_nxt.buf_syn <= '0';
        ctrl_nxt.state <= S_IDLE;

      WHEN S_DOWNLOAD_START => -- start block download / send single request (if no bursts)
        -- ------------------------------------------------------------
        bus_req_o.addr <= ctrl.tag_idx & ctrl.ofs_ext(offset_width_c - 1 DOWNTO 0) & "00";
        bus_req_o.rw <= '0'; -- read access
        bus_req_o.stb <= '1'; -- send (initial burst/locking) request
        bus_req_o.lock <= '1'; -- this is a locked transfer
        bus_req_o.burst <= bool_to_ulogic_f(bursts_en_c); -- this is a burst transfer
        bus_req_o.ben <= (OTHERS => '1'); -- full-word access
        ctrl_nxt.state <= S_DOWNLOAD_WAIT;

      WHEN S_DOWNLOAD_WAIT => -- wait for exclusive/locked bus access
        -- ------------------------------------------------------------
        cache_o.addr <= ctrl.tag_idx & ctrl.ofs_int & "00";
        cache_o.data <= bus_rsp_i.data;
        cache_o.we <= (OTHERS => '1'); -- just keep writing full words
        bus_req_o.addr <= ctrl.tag_idx & ctrl.ofs_ext(offset_width_c - 1 DOWNTO 0) & "00";
        bus_req_o.rw <= '0'; -- read access
        bus_req_o.lock <= '1'; -- this is a locked transfer
        bus_req_o.burst <= bool_to_ulogic_f(bursts_en_c); -- this is a burst transfer
        bus_req_o.ben <= (OTHERS => '1'); -- full-word access
        -- wait for initial ACK to start actual bursting --
        IF (bus_rsp_i.ack = '1') THEN
          ctrl_nxt.bus_err <= ctrl.bus_err OR bus_rsp_i.err; -- accumulate bus errors
          ctrl_nxt.ofs_int <= STD_ULOGIC_VECTOR(unsigned(ctrl.ofs_int) + 1);
          ctrl_nxt.ofs_ext <= STD_ULOGIC_VECTOR(unsigned(ctrl.ofs_ext) + 1);
          IF bursts_en_c THEN
            ctrl_nxt.state <= S_DOWNLOAD_RUN;
          ELSIF (and_reduce_f(ctrl.ofs_int) = '1') THEN -- block completed
            ctrl_nxt.state <= S_DONE;
          ELSE
            ctrl_nxt.state <= S_DOWNLOAD_START;
          END IF;
        END IF;

      WHEN S_DOWNLOAD_RUN => -- bursts enabled: send read requests and get data responses
        -- ------------------------------------------------------------
        IF bursts_en_c THEN
          cache_o.addr <= ctrl.tag_idx & ctrl.ofs_int & "00";
          cache_o.data <= bus_rsp_i.data;
          cache_o.we <= (OTHERS => '1'); -- just keep writing full words
          bus_req_o.addr <= ctrl.tag_idx & ctrl.ofs_ext(offset_width_c - 1 DOWNTO 0) & "00";
          bus_req_o.rw <= '0'; -- read access
          bus_req_o.lock <= '1'; -- this is a locked transfer
          bus_req_o.burst <= '1'; -- this is a burst transfer
          bus_req_o.ben <= (OTHERS => '1'); -- full-word access
          -- send requests --
          IF (ctrl.ofs_ext(offset_width_c) = '0') THEN
            ctrl_nxt.ofs_ext <= STD_ULOGIC_VECTOR(unsigned(ctrl.ofs_ext) + 1); -- next cache word
            bus_req_o.stb <= '1'; -- request next transfer
          END IF;
          -- receive responses --
          IF (bus_rsp_i.ack = '1') THEN
            ctrl_nxt.bus_err <= ctrl.bus_err OR bus_rsp_i.err; -- accumulate bus errors
            ctrl_nxt.ofs_int <= STD_ULOGIC_VECTOR(unsigned(ctrl.ofs_int) + 1); -- next main memory location
            IF (and_reduce_f(ctrl.ofs_int) = '1') THEN -- block completed
              ctrl_nxt.state <= S_DONE;
            END IF;
          END IF;
        ELSE -- single transfers only
          ctrl_nxt.state <= S_IDLE;
        END IF;

      WHEN S_DONE => -- any error during block update?
        -- ------------------------------------------------------------
        IF (ctrl.bus_err = '0') THEN
          cache_o.cmd_new <= '1'; -- set tag and make valid
          ctrl_nxt.state <= S_DELAY;
        ELSE
          host_rsp_o.ack <= '1';
          host_rsp_o.err <= '1';
          ctrl_nxt.state <= S_IDLE;
        END IF;

      WHEN S_DELAY => -- delay cycle for cache status update
        -- ------------------------------------------------------------
        ctrl_nxt.state <= S_CHECK;

      WHEN OTHERS => -- undefined
        -- ------------------------------------------------------------
        ctrl_nxt.state <= S_IDLE;

    END CASE;
  END PROCESS ctrl_engine_comb;
  -- Cache Bypass (Direct Access) Response Buffer -------------------------------------------
  -- -------------------------------------------------------------------------------------------
  response_buf : PROCESS (rstn_i, clk_i)
  BEGIN
    IF (rstn_i = '0') THEN
      bp_rsp <= rsp_terminate_c;
    ELSIF rising_edge(clk_i) THEN
      bp_rsp.ack <= bus_rsp_i.ack;
      bp_rsp.err <= bus_rsp_i.err;
      IF (bus_rsp_i.ack = '1') THEN -- reduce switching activity
        bp_rsp.data <= bus_rsp_i.data;
      END IF;
    END IF;
  END PROCESS response_buf;
  -- Status Memory --------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  status_mem_large :
  IF (NUM_BLOCKS > 1) GENERATE
    status_memory : PROCESS (rstn_i, clk_i)
    BEGIN
      IF (rstn_i = '0') THEN
        valid <= (OTHERS => '0');
        valid_rd <= '0';
      ELSIF rising_edge(clk_i) THEN
        IF (cache_o.cmd_clr = '1') THEN -- invalidate entire cache
          valid <= (OTHERS => '0');
        ELSIF (cache_o.cmd_new = '1') THEN -- make indexed block valid
          valid(to_integer(unsigned(cache_o.addr(31 - tag_width_c DOWNTO 2 + offset_width_c)))) <= '1';
        END IF;
        valid_rd <= valid(to_integer(unsigned(cache_o.addr(31 - tag_width_c DOWNTO 2 + offset_width_c))));
      END IF;
    END PROCESS status_memory;
  END GENERATE;

  -- single-entry only --
  status_mem_small :
  IF (NUM_BLOCKS = 1) GENERATE
    status_memory : PROCESS (rstn_i, clk_i)
    BEGIN
      IF (rstn_i = '0') THEN
        valid(0) <= '0';
      ELSIF rising_edge(clk_i) THEN
        IF (cache_o.cmd_clr = '1') THEN -- invalidate
          valid(0) <= '0';
        ELSIF (cache_o.cmd_new = '1') THEN -- make valid
          valid(0) <= '1';
        END IF;
      END IF;
    END PROCESS status_memory;
    valid_rd <= valid(0);
  END GENERATE;
  -- Cache Hit Check ------------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  tag_buffer : PROCESS (rstn_i, clk_i)
  BEGIN
    IF (rstn_i = '0') THEN
      tag_reg <= (OTHERS => '0');
    ELSIF rising_edge(clk_i) THEN
      tag_reg <= cache_o.addr(31 DOWNTO 31 - (tag_width_c - 1));
    END IF;
  END PROCESS tag_buffer;

  -- cache hit --
  cache_i.hit <= '1' WHEN (valid_rd = '1') AND (tag_rd(tag_width_c - 1 DOWNTO 0) = tag_reg) ELSE
  '0';
  -- Cache Tag and Data Memory (Wrapper) ----------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  neorv32_cache_ram_inst : neorv32_cache_ram
  GENERIC MAP(
    TAG_WIDTH => tag_width_c,
    IDX_WIDTH => index_width_c,
    OFS_WIDTH => offset_width_c
  )
  PORT MAP(
    clk_i => clk_i,
    addr_i => cache_o.addr,
    tag_we_i => cache_o.cmd_new,
    tag_o => tag_rd,
    data_we_i => cache_o.we,
    data_i => cache_o.data,
    data_o => cache_i.data
  );

END neorv32_cache_rtl;