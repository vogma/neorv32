-- ================================================================================ --
-- VUnit Testbench Shell for neorv32_cache                                          --
-- -------------------------------------------------------------------------------- --
-- Provides harness, clock/reset generation, helper procedures, and an empty        --
-- test runner. Add test cases as VUnit `run` blocks.                               --
-- ================================================================================ --

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY neorv32;
USE neorv32.neorv32_package.ALL;

LIBRARY vunit_lib;
CONTEXT vunit_lib.vunit_context;

ENTITY tb_cache IS
  GENERIC (
    runner_cfg : STRING;
    NUM_BLOCKS : NATURAL := 4;
    BLOCK_SIZE : NATURAL := 8;
    BURSTS_EN : BOOLEAN := false;
    READ_ONLY : BOOLEAN := false;
    UC_BEGIN : STD_ULOGIC_VECTOR(3 DOWNTO 0) := x"F"
  );
END ENTITY;

ARCHITECTURE tb OF tb_cache IS

  CONSTANT CLK_PERIOD : TIME := 10 ns;

  -- DUT signals --
  SIGNAL clk : STD_ULOGIC := '0';
  SIGNAL rstn : STD_ULOGIC := '0';
  SIGNAL host_req : bus_req_t;
  SIGNAL host_rsp : bus_rsp_t;
  SIGNAL bus_req : bus_req_t;
  SIGNAL bus_rsp : bus_rsp_t;

  SIGNAL bus_activity, bus_stb_clear : STD_LOGIC := '0';

  SIGNAL cache_err : STD_LOGIC;

  -- memory model --
  SIGNAL force_err : STD_ULOGIC := '0';
  SIGNAL inspect_addr : STD_ULOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
  SIGNAL inspect_data : STD_ULOGIC_VECTOR(31 DOWNTO 0);

  SIGNAL init_we   : STD_ULOGIC := '0';
  SIGNAL init_addr : STD_ULOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
  SIGNAL init_data : STD_ULOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');

  -- ======================================================================== --
  -- Helper procedures                                                        --
  -- ======================================================================== --

  -- Wait for N clock rising edges --
  PROCEDURE wait_cycles(SIGNAL clk_s : IN STD_ULOGIC; n : NATURAL) IS
  BEGIN
    FOR i IN 1 TO n LOOP
      WAIT UNTIL rising_edge(clk_s);
    END LOOP;
  END PROCEDURE;

  -- Issue a cache read and return the data --
  PROCEDURE cache_read(
    SIGNAL clk_s : IN STD_ULOGIC;
    SIGNAL req : INOUT bus_req_t;
    SIGNAL rsp : IN bus_rsp_t;
    addr : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    data : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0);
    err : OUT STD_ULOGIC
  ) IS
  BEGIN
    WAIT UNTIL rising_edge(clk_s);
    req <= req_terminate_c;
    req.addr <= addr;
    req.ben <= "1111";
    req.stb <= '1';
    req.rw <= '0';
    WAIT UNTIL rising_edge(clk_s);
    req.stb <= '0';
    -- wait for ack --
    LOOP
      WAIT UNTIL rising_edge(clk_s);
      IF rsp.ack = '1' THEN
        data := rsp.data;
        err := rsp.err;
        RETURN;
      END IF;
    END LOOP;
  END PROCEDURE;

  -- Issue an atomic (AMO) cache read — bypasses cache --
  PROCEDURE cache_read_amo(
    SIGNAL clk_s : IN STD_ULOGIC;
    SIGNAL req : INOUT bus_req_t;
    SIGNAL rsp : IN bus_rsp_t;
    addr : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    data : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0);
    err : OUT STD_ULOGIC
  ) IS
  BEGIN
    WAIT UNTIL rising_edge(clk_s);
    req <= req_terminate_c;
    req.addr <= addr;
    req.ben <= "1111";
    req.stb <= '1';
    req.rw <= '0';
    req.amo <= '1';
    WAIT UNTIL rising_edge(clk_s);
    req.stb <= '0';
    req.amo <= '0';
    -- wait for ack --
    LOOP
      WAIT UNTIL rising_edge(clk_s);
      IF rsp.ack = '1' THEN
        data := rsp.data;
        err := rsp.err;
        RETURN;
      END IF;
    END LOOP;
  END PROCEDURE;

  -- Issue a cache write --
  PROCEDURE cache_write(
    SIGNAL clk_s : IN STD_ULOGIC;
    SIGNAL req : INOUT bus_req_t;
    SIGNAL rsp : IN bus_rsp_t;
    addr : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    data : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    ben : IN STD_ULOGIC_VECTOR(3 DOWNTO 0);
    err : OUT STD_ULOGIC
  ) IS
  BEGIN
    WAIT UNTIL rising_edge(clk_s);
    req <= req_terminate_c;
    req.addr <= addr;
    req.data <= data;
    req.ben <= ben;
    req.stb <= '1';
    req.rw <= '1';
    WAIT UNTIL rising_edge(clk_s);
    req.stb <= '0';
    -- wait for ack --
    LOOP
      WAIT UNTIL rising_edge(clk_s);
      IF rsp.ack = '1' THEN
        err := rsp.err;
        RETURN;
      END IF;
    END LOOP;
  END PROCEDURE;

  -- Issue a fence (cache invalidation) --
  PROCEDURE cache_fence(
    SIGNAL clk_s : IN STD_ULOGIC;
    SIGNAL req : INOUT bus_req_t
  ) IS
  BEGIN
    WAIT UNTIL rising_edge(clk_s);
    req <= req_terminate_c;
    req.fence <= '1';
    WAIT UNTIL rising_edge(clk_s);
    req.fence <= '0';
  END PROCEDURE;

  -- Read memory model directly via inspection port --
  PROCEDURE mem_inspect(
    SIGNAL iaddr : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL idata : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    addr : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    data : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0)
  ) IS
  BEGIN
    iaddr <= addr;
    WAIT FOR 1 ns; -- combinational settle
    data := idata;
  END PROCEDURE;

  -- Seed memory model directly (bypasses cache) --
  PROCEDURE mem_seed(
    SIGNAL clk_s  : IN  STD_ULOGIC;
    SIGNAL we     : OUT STD_ULOGIC;
    SIGNAL addr_s : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL data_s : OUT STD_ULOGIC_VECTOR(31 DOWNTO 0);
    addr : IN STD_ULOGIC_VECTOR(31 DOWNTO 0);
    data : IN STD_ULOGIC_VECTOR(31 DOWNTO 0)
  ) IS
  BEGIN
    WAIT UNTIL rising_edge(clk_s);
    we     <= '1';
    addr_s <= addr;
    data_s <= data;
    WAIT UNTIL rising_edge(clk_s);
    we     <= '0';
  END PROCEDURE;

BEGIN

  -- ======================================================================== --
  -- Clock and reset                                                          --
  -- ======================================================================== --
  clk <= NOT clk AFTER CLK_PERIOD / 2;
  rstn <= '1' AFTER CLK_PERIOD * 5;

  -- ======================================================================== --
  -- DUT                                                                      --
  -- ======================================================================== --
  dut : ENTITY neorv32.neorv32_cache
    GENERIC MAP(
      NUM_BLOCKS => NUM_BLOCKS,
      BLOCK_SIZE => BLOCK_SIZE,
      UC_BEGIN => UC_BEGIN,
      READ_ONLY => READ_ONLY,
      BURSTS_EN => BURSTS_EN
    )
    PORT MAP(
      clk_i => clk,
      rstn_i => rstn,
      host_req_i => host_req,
      host_rsp_o => host_rsp,
      bus_req_o => bus_req,
      bus_rsp_i => bus_rsp
    );

  -- ======================================================================== --
  -- Memory model                                                             --
  -- ======================================================================== --
  mem : ENTITY neorv32.bus_mem_model
    GENERIC MAP(
      MEM_SIZE => 4096,
      LATENCY => 1
    )
    PORT MAP(
      clk_i => clk,
      rstn_i => rstn,
      bus_req_i => bus_req,
      bus_rsp_o => bus_rsp,
      force_err_i => force_err,
      inspect_addr_i => inspect_addr,
      inspect_data_o => inspect_data,
      init_we_i      => init_we,
      init_addr_i    => init_addr,
      init_data_i    => init_data
    );

  -- Monitor xbus traffic
  PROCESS (clk) BEGIN
    IF (rising_edge(clk)) THEN
      IF (host_req.stb = '1') THEN
        bus_activity <= '0';
      ELSIF bus_req.stb = '1' THEN
        bus_activity <= '1';
      END IF;
    END IF;
  END PROCESS;

  -- ======================================================================== --
  -- VUnit test runner                                                        --
  -- ======================================================================== --
  test_runner : PROCESS
    VARIABLE rd_data : STD_ULOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE rd_err : STD_ULOGIC;
    VARIABLE wr_data : STD_ULOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE test_addr : unsigned(31 DOWNTO 0);
  BEGIN
    test_runner_setup(runner, runner_cfg);

    -- wait for reset release --
    WAIT UNTIL rstn = '1';
    wait_cycles(clk, 2);

    -- ----------------------------------------------------------------
    -- Smoke test: write a word to memory (write-through), read it
    -- back through the cache. Verifies basic bus handshake.
    -- ----------------------------------------------------------------
    IF run("read_write_test") THEN

      wr_data := x"0000000F";
      test_addr := x"00000000";
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), wr_data, "1111", rd_err);
      check_equal(rd_err, '0', "write error");
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "cache read data mismatch");

      -- mem_inspect(inspect_addr,)
    END IF;
    -- ----------------------------------------------------------------
    -- Group 1: read_miss_then_hit
    -- Read address A (miss -> block fill from memory). Verify data.
    -- Read address A again (hit -> no bus activity). Verify same data.
    -- ----------------------------------------------------------------
    IF run("read_miss_then_hit") THEN
      wr_data := x"0000000F";
      test_addr := x"00000000";

      -- Seed memory directly (bypass cache)
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);

      -- First read: miss -> block fill
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "first read should be a miss");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "cache read data mismatch");

      -- Second read: hit -> no bus traffic
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "second read should be a hit");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "cache read data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 1: read_block_words
    -- Read first word of a block (miss -> fill entire block).
    -- Read remaining words in same block -- all should be hits.
    -- Verify each word returns correct data from memory.
    -- ----------------------------------------------------------------
    IF run("read_block_words") THEN
      wr_data := x"0000000F";
      test_addr := x"00000000";

      -- Seed all words in one block directly
      FOR i IN 0 TO BLOCK_SIZE/4 - 1 LOOP
        mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);
        test_addr := test_addr + 4;
      END LOOP;
      test_addr := x"00000000";

      -- First word read: miss -> fills entire block
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "first word should be a miss");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "cache read data mismatch");

      -- Remaining words: all hits (same cache line was filled on first read)
      FOR i IN 1 TO BLOCK_SIZE/4 - 1 LOOP
        test_addr := test_addr + 4;
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(bus_activity, '0', "block word " & INTEGER'image(i) & " should be a hit");
        check_equal(rd_err, '0', "read error");
      END LOOP;
    END IF;

    -- ----------------------------------------------------------------
    -- Group 2: write_hit
    -- Pre-fill a cache line via read (miss -> fill).
    -- Write new data to same address (write-through: updates cache
    -- AND memory). Re-read: verify cache returns updated data.
    -- mem_inspect: verify memory also has the updated data.
    -- ----------------------------------------------------------------
    IF run("write_hit") THEN
      wr_data := x"0000000F";
      test_addr := x"00000000";

      -- Pre-fill cache line via read (miss -> fill)
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "pre-fill should be a miss");
      check_equal(rd_err, '0', "read error");

      -- Verify entire block is now cached
      FOR i IN 1 TO BLOCK_SIZE/4 - 1 LOOP
        test_addr := test_addr + 4;
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(bus_activity, '0', "block word " & INTEGER'image(i) & " should be a hit");
        check_equal(rd_err, '0', "read error");
      END LOOP;

      test_addr := x"00000000";

      -- Write to cached address (write-through)
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), wr_data, "1111", rd_err);

      -- Re-read: cache should return updated data (hit)
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "re-read after write should be a hit");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "cache read data mismatch");

      -- Verify write-through: data must also be in memory
      mem_inspect(inspect_addr, inspect_data, STD_ULOGIC_VECTOR(test_addr), rd_data);
      check_equal(rd_data, wr_data, "write-through: data should be in memory");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 2: write_miss
    -- Write to address not in cache (miss -> write-through to memory,
    -- no cache line fill). Verify memory updated via mem_inspect.
    -- Read same address: should trigger miss + fill. Verify data
    -- matches what was written.
    -- ----------------------------------------------------------------
    IF run("write_miss") THEN
      wr_data := x"0000000F";
      test_addr := x"00000000";

      -- Write miss: goes directly to memory, no cache fill
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), wr_data, "1111", rd_err);

      -- Verify data reached memory
      mem_inspect(inspect_addr, inspect_data, STD_ULOGIC_VECTOR(test_addr), rd_data);
      check_equal(rd_data, wr_data, "write-through: data should be in memory");

      -- Read triggers miss + fill (write miss did not fill the line)
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "read after write-miss should be a miss");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "cache read data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 2: byte_write
    -- Write a full word to memory, then overwrite a single byte
    -- using ben="0100" (byte 2 only). Read back through cache:
    -- verify only the targeted byte changed.
    -- ----------------------------------------------------------------
    IF run("byte_write") THEN
      test_addr := x"00000000";

      -- Seed full word to memory directly
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), STD_ULOGIC_VECTOR'(x"12345678"));

      -- Overwrite only byte 2 (bits 23:16) via partial ben
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), STD_ULOGIC_VECTOR'(x"FFFFFFFF"), "0100", rd_err);

      -- Read back: miss -> fill from memory; only byte 2 should differ
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "read should be a miss");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"12FF5678"), "only byte 2 should have changed");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 3: eviction
    -- Fill cache line at index 0 with tag A (read addr_A -> fill).
    -- Read addr_B that maps to same index but different tag ->
    -- evicts A, fills B. Verify B data is correct.
    -- Re-read addr_A -> should be a miss (refill from memory).
    -- Addresses offset by NUM_BLOCKS * BLOCK_SIZE to collide on
    -- the same index with a different tag.
    -- ----------------------------------------------------------------
    IF run("eviction") THEN
      test_addr := x"00000000"; -- addr A: index 0, tag 0
      wr_data := x"AABBCCDD";

      -- Seed addr A in memory directly
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);

      -- Seed addr B in memory directly
      test_addr := to_unsigned(NUM_BLOCKS * BLOCK_SIZE, 32); -- addr B: index 0, tag 1
      wr_data := x"11223344";
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);

      -- Read addr A -> miss, fills cache line
      test_addr := x"00000000";
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "read A: expected miss");
      check_equal(rd_err, '0', "read A error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"AABBCCDD"), "read A data mismatch");

      -- Read addr A again -> hit (line is cached)
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "read A again: expected hit");
      check_equal(rd_err, '0', "read A again error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"AABBCCDD"), "read A again data mismatch");

      -- Read addr B -> miss, evicts A from index 0, fills B
      test_addr := to_unsigned(NUM_BLOCKS * BLOCK_SIZE, 32);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "read B: expected miss (evicts A)");
      check_equal(rd_err, '0', "read B error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"11223344"), "read B data mismatch");

      -- Read addr B again -> hit (B is now cached)
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "read B again: expected hit");
      check_equal(rd_err, '0', "read B again error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"11223344"), "read B again data mismatch");

      -- Read addr A -> miss (A was evicted, no write-back for write-through)
      test_addr := x"00000000";
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "read A after eviction: expected miss");
      check_equal(rd_err, '0', "read A after eviction error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"AABBCCDD"), "read A after eviction data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 4: fence_invalidates
    -- Fill all cache lines via reads (one per index).
    -- Verify all lines are cached (hits). Issue cache_fence.
    -- Re-read all addresses: every one should be a miss (refill
    -- from memory). Verify data still correct after refill.
    -- ----------------------------------------------------------------
    IF run("fence_invalidates") THEN
      -- Fill all cache lines at distinct indices
      FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
        test_addr := to_unsigned(i * BLOCK_SIZE, 32);
        wr_data := STD_ULOGIC_VECTOR(to_unsigned(16#A0A0# + i, 32));
        -- Seed memory directly
        mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);
        -- Read to fill cache line
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(rd_err, '0', "fill read error line " & INTEGER'image(i));
      END LOOP;

      -- Verify all lines are cached (hits)
      FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
        test_addr := to_unsigned(i * BLOCK_SIZE, 32);
        wait_cycles(clk, 2);
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(bus_activity, '0', "pre-fence: line " & INTEGER'image(i) & " should be a hit");
        check_equal(rd_err, '0', "pre-fence read error line " & INTEGER'image(i));
      END LOOP;

      -- Issue fence -> invalidates entire cache
      cache_fence(clk, host_req);
      wait_cycles(clk, 2);

      -- All reads should now be misses
      FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
        test_addr := to_unsigned(i * BLOCK_SIZE, 32);
        wr_data := STD_ULOGIC_VECTOR(to_unsigned(16#A0A0# + i, 32));
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(bus_activity, '1', "post-fence: line " & INTEGER'image(i) & " should be a miss");
        check_equal(rd_err, '0', "post-fence read error line " & INTEGER'image(i));
        check_equal(rd_data, wr_data, "post-fence data mismatch line " & INTEGER'image(i));
      END LOOP;
    END IF;

    -- ----------------------------------------------------------------
    -- Group 5: uncached_read_write
    -- Read/write to address in uncached region (addr >= UC_BEGIN<<28,
    -- e.g. 0xF0000000). Verify request goes through (ack received),
    -- no cache fill. Memory model wraps address to low 12 bits.
    -- ----------------------------------------------------------------
    IF run("uncached_read_write") THEN
      test_addr := x"00000000";

      -- Seed memory at physical address 0 directly
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), STD_ULOGIC_VECTOR'(x"DEADBEEF"));

      -- Seed memory at physical address 4
      test_addr := x"00000004";
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), STD_ULOGIC_VECTOR'(x"12345678"));

      -- Uncached read from 0xF0000000 (wraps to physical byte 0)
      test_addr := x"F0000000";
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "uncached read should go to bus");
      check_equal(rd_err, '0', "uncached read error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"DEADBEEF"), "uncached read data mismatch");

      -- Second uncached read: should still go to bus (no cache fill)
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "second uncached read should still go to bus (no fill)");
      check_equal(rd_err, '0', "second uncached read error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"DEADBEEF"), "second uncached read data mismatch");

      -- Uncached write to 0xF0000004 (wraps to physical byte 4)
      test_addr := x"F0000004";
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), STD_ULOGIC_VECTOR'(x"CAFEBABE"), "1111", rd_err);
      check_equal(rd_err, '0', "uncached write error");

      -- Verify write reached memory
      mem_inspect(inspect_addr, inspect_data, STD_ULOGIC_VECTOR'(x"00000004"), rd_data);
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"CAFEBABE"), "uncached write: data should be in memory");

      -- Uncached read-back from 0xF0000004
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "uncached read-back should go to bus");
      check_equal(rd_err, '0', "uncached read-back error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"CAFEBABE"), "uncached read-back data mismatch");

      -- Verify no cache pollution: cached read to 0x00000000 should miss
      test_addr := x"00000000";
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "cached read after uncached ops should be a miss");
      check_equal(rd_err, '0', "cached read error");
      check_equal(rd_data, STD_ULOGIC_VECTOR'(x"DEADBEEF"), "cached read data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 5: atomic_bypass
    -- Issue a read with amo='1' to a cacheable address. Verify the
    -- access bypasses the cache (goes to bus) and does not disturb
    -- existing cached state.
    -- ----------------------------------------------------------------
    IF run("atomic_bypass") THEN
      test_addr := x"00000000";
      wr_data := x"AABBCCDD";

      -- Seed memory directly
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);

      -- Normal read: miss -> fills cache line
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "first read should be a miss");
      check_equal(rd_err, '0', "first read error");
      check_equal(rd_data, wr_data, "first read data mismatch");

      -- Normal re-read: hit (line is cached)
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "re-read should be a hit");
      check_equal(rd_err, '0', "re-read error");
      check_equal(rd_data, wr_data, "re-read data mismatch");

      -- AMO read: bypasses cache, goes to bus
      cache_read_amo(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "AMO read should bypass cache (go to bus)");
      check_equal(rd_err, '0', "AMO read error");
      check_equal(rd_data, wr_data, "AMO read data mismatch");

      -- Normal read after AMO: should still be a hit (AMO didn't invalidate)
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "read after AMO should still be a hit");
      check_equal(rd_err, '0', "read after AMO error");
      check_equal(rd_data, wr_data, "read after AMO data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 6: bus_error_during_fill
    -- Set force_err='1' on memory model. Issue a read (miss -> fill).
    -- Bus errors are accumulated; S_DONE sees bus_err='1', returns
    -- err to host WITHOUT validating the cache line.
    -- Deassert force_err. Re-read same address: should be a miss
    -- (line was not validated). Verify second read succeeds.
    -- ----------------------------------------------------------------
    IF run("bus_error_during_fill") THEN
      test_addr := x"00000000";
      wr_data := x"DEADBEEF";

      -- Seed memory directly
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);

      -- Fence to clear cache
      cache_fence(clk, host_req);
      wait_cycles(clk, 2);

      -- Enable error injection
      force_err <= '1';

      -- Read triggers miss -> fill; all bus responses carry err
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(rd_err, '1', "fill with bus error should propagate err to host");

      -- Disable error injection
      force_err <= '0';
      wait_cycles(clk, 2);

      -- Re-read: line was NOT validated, so this should be a miss
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '1', "read after failed fill should be a miss");
      check_equal(rd_err, '0', "second read should succeed");
      check_equal(rd_data, wr_data, "second read data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 6: bus_error_on_write
    -- Set force_err='1'. Issue a write (write-through to bus).
    -- Verify err='1' returned to host. Deassert force_err, verify
    -- a subsequent write succeeds.
    -- ----------------------------------------------------------------
    IF run("bus_error_on_write") THEN
      test_addr := x"00000000";
      wr_data := x"CAFEBABE";

      -- Enable error injection
      force_err <= '1';

      -- Write goes write-through to bus; memory model returns err
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), wr_data, "1111", rd_err);
      check_equal(rd_err, '1', "write with bus error should propagate err to host");

      -- Disable error injection
      force_err <= '0';
      wait_cycles(clk, 2);

      -- Subsequent write should succeed
      cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), wr_data, "1111", rd_err);
      check_equal(rd_err, '0', "write after clearing force_err should succeed");

      -- Verify data reached memory
      mem_inspect(inspect_addr, inspect_data, STD_ULOGIC_VECTOR(test_addr), rd_data);
      check_equal(rd_data, wr_data, "write-through data should be in memory");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 7: fence_during_pending_request
    -- Pre-fill a cache line so it hits. Then drive fence and read
    -- request on the SAME clock edge. The FSM must process the fence
    -- first (S_CLEAR) before the read. The read should therefore be
    -- a miss.
    -- ----------------------------------------------------------------
    IF run("fence_during_pending_request") THEN
      test_addr := x"00000000";
      wr_data := x"AABBCCDD";

      -- Seed memory directly, then fill cache line via read
      mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(rd_err, '0', "fill read error");

      -- Verify line is cached (hit)
      wait_cycles(clk, 2);
      cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
      check_equal(bus_activity, '0', "pre-fence read should be a hit");
      check_equal(rd_err, '0', "pre-fence read error");
      check_equal(rd_data, wr_data, "pre-fence read data mismatch");

      -- Drive fence one cycle before stb so that buf_syn is registered
      -- when stb arrives. On that edge the FSM sees both buf_syn='1'
      -- (registered) and host_req_i.stb='1' (live); fence must win.
      WAIT UNTIL rising_edge(clk);
      host_req <= req_terminate_c;
      host_req.fence <= '1';
      -- Next cycle: deassert fence, assert read request
      WAIT UNTIL rising_edge(clk);
      host_req.fence <= '0';
      host_req.addr <= STD_ULOGIC_VECTOR(test_addr);
      host_req.ben <= "1111";
      host_req.stb <= '1';
      host_req.rw <= '0';
      WAIT UNTIL rising_edge(clk);
      host_req.stb <= '0';

      -- Wait for ack from the read
      LOOP
        WAIT UNTIL rising_edge(clk);
        IF host_rsp.ack = '1' THEN
          rd_data := host_rsp.data;
          rd_err := host_rsp.err;
          EXIT;
        END IF;
      END LOOP;

      -- Read must be a miss: fence invalidated the line before the read was processed
      check_equal(bus_activity, '1', "read after simultaneous fence should be a miss");
      check_equal(rd_err, '0', "read error");
      check_equal(rd_data, wr_data, "read data mismatch");
    END IF;

    -- ----------------------------------------------------------------
    -- Group 8: sequential_fill_and_verify
    -- Write NUM_BLOCKS distinct values to different cache lines.
    -- Read them all back (misses -> fills). Read again (all hits).
    -- Exercises full index range and confirms no inter-line corruption.
    -- ----------------------------------------------------------------
    IF run("sequential_fill_and_verify") THEN

      -- Seed memory with distinct values at each cache-line index
      FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
        test_addr := to_unsigned(i * BLOCK_SIZE, 32);
        wr_data := x"A000" & STD_ULOGIC_VECTOR(to_unsigned(i, 16));
        mem_seed(clk, init_we, init_addr, init_data, STD_ULOGIC_VECTOR(test_addr), wr_data);
      END LOOP;

      -- Read pass 1: every line should be a miss (fill from memory)
      FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
        test_addr := to_unsigned(i * BLOCK_SIZE, 32);
        wr_data := x"A000" & STD_ULOGIC_VECTOR(to_unsigned(i, 16));
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(bus_activity, '1', "pass 1: line " & INTEGER'image(i) & " should be a miss");
        check_equal(rd_err, '0', "pass 1: read error line " & INTEGER'image(i));
        check_equal(rd_data, wr_data, "pass 1: data mismatch line " & INTEGER'image(i));
      END LOOP;

      -- Read pass 2: every line should be a hit
      FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
        test_addr := to_unsigned(i * BLOCK_SIZE, 32);
        wr_data := x"A000" & STD_ULOGIC_VECTOR(to_unsigned(i, 16));
        wait_cycles(clk, 2);
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rd_data, rd_err);
        check_equal(bus_activity, '0', "pass 2: line " & INTEGER'image(i) & " should be a hit");
        check_equal(rd_err, '0', "pass 2: read error line " & INTEGER'image(i));
        check_equal(rd_data, wr_data, "pass 2: data mismatch line " & INTEGER'image(i));
      END LOOP;

    END IF;

    test_runner_cleanup(runner);
  END PROCESS;

  -- VUnit watchdog: fail if test hangs --
  test_runner_watchdog(runner, 1 ms);

END ARCHITECTURE;