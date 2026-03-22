-- ================================================================================ --
-- VUnit + OSVVM Full-Chain Testbench Shell for neorv32_cache_wb                    --
-- -------------------------------------------------------------------------------- --
-- Instantiates the full chain: cache → XBUS gateway → AXI bridge → OSVVM          --
-- Axi4Memory. Exercises write-back eviction paths under realistic AXI latency.     --
-- ================================================================================ --

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

LIBRARY neorv32;
USE neorv32.neorv32_package.ALL;

LIBRARY osvvm;
CONTEXT osvvm.OsvvmContext;

LIBRARY osvvm_axi4;
CONTEXT osvvm_axi4.Axi4Context;

LIBRARY vunit_lib;
CONTEXT vunit_lib.vunit_context;

ENTITY tb_cache_chain IS
  GENERIC (
    runner_cfg  : STRING;
    BURSTS_EN   : BOOLEAN := true;
    REGSTAGE_EN : BOOLEAN := false
  );
END ENTITY;

ARCHITECTURE tb OF tb_cache_chain IS

  CONSTANT CLK_PERIOD : TIME := 10 ns; -- 100 MHz

  -- Fixed cache parameters --
  CONSTANT NUM_BLOCKS : NATURAL := 4;
  CONSTANT BLOCK_SIZE : NATURAL := 16; -- 16 bytes = 4 words per line
  CONSTANT UC_BEGIN   : STD_ULOGIC_VECTOR(3 DOWNTO 0) := x"F";
  CONSTANT BURST_LEN  : NATURAL := BLOCK_SIZE; -- bridge BURST_LEN must equal cache BLOCK_SIZE

  -- Address geometry (integer constants to avoid GHDL natural*unsigned width issues) --
  CONSTANT BASE_ADDR     : unsigned(31 DOWNTO 0) := x"10000000";
  CONSTANT EVICT_STRIDE  : NATURAL := NUM_BLOCKS * BLOCK_SIZE; -- 0x40
  CONSTANT BLOCK_STRIDE  : NATURAL := BLOCK_SIZE;              -- 0x10
  CONSTANT WORDS_PER_BLK : NATURAL := BLOCK_SIZE / 4;          -- 4

  -- AXI widths --
  CONSTANT AXI_ADDR_WIDTH : INTEGER := 32;
  CONSTANT AXI_DATA_WIDTH : INTEGER := 32;
  CONSTANT AXI_ID_WIDTH   : INTEGER := 4;
  CONSTANT AXI_USER_WIDTH : INTEGER := 1;

  -- ======================================================================== --
  -- Signal declarations                                                      --
  -- ======================================================================== --

  -- 1. Host-side (testbench → cache) --
  SIGNAL clk      : STD_ULOGIC := '0';
  SIGNAL rstn     : STD_ULOGIC := '0';
  SIGNAL host_req : bus_req_t;
  SIGNAL host_rsp : bus_rsp_t;

  -- 2. Internal bus (cache → XBUS gateway) --
  SIGNAL cache_bus_req : bus_req_t;
  SIGNAL cache_bus_rsp : bus_rsp_t;

  -- 3. XBUS flat signals (XBUS gateway → bridge) --
  SIGNAL xbus_adr   : STD_ULOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL xbus_dat_o : STD_ULOGIC_VECTOR(31 DOWNTO 0); -- gateway write data out
  SIGNAL xbus_dat_i : STD_ULOGIC_VECTOR(31 DOWNTO 0); -- bridge read data back
  SIGNAL xbus_cti   : STD_ULOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL xbus_tag   : STD_ULOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL xbus_we    : STD_ULOGIC;
  SIGNAL xbus_sel   : STD_ULOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL xbus_stb   : STD_ULOGIC;
  SIGNAL xbus_cyc   : STD_ULOGIC;
  SIGNAL xbus_ack   : STD_ULOGIC;
  SIGNAL xbus_err   : STD_ULOGIC;

  -- 4. AXI4 flat signals (bridge → OSVVM binding) --
  SIGNAL m_axi_awaddr  : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL m_axi_awlen   : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL m_axi_awsize  : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL m_axi_awburst : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL m_axi_awcache : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL m_axi_awprot  : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL m_axi_awvalid : STD_LOGIC;
  SIGNAL m_axi_awready : STD_LOGIC;

  SIGNAL m_axi_wdata  : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL m_axi_wstrb  : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL m_axi_wlast  : STD_LOGIC;
  SIGNAL m_axi_wvalid : STD_LOGIC;
  SIGNAL m_axi_wready : STD_LOGIC;

  SIGNAL m_axi_bresp  : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL m_axi_bvalid : STD_LOGIC;
  SIGNAL m_axi_bready : STD_LOGIC;

  SIGNAL m_axi_araddr  : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL m_axi_arlen   : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL m_axi_arsize  : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL m_axi_arburst : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL m_axi_arcache : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL m_axi_arprot  : STD_LOGIC_VECTOR(2 DOWNTO 0);
  SIGNAL m_axi_arvalid : STD_LOGIC;
  SIGNAL m_axi_arready : STD_LOGIC;

  SIGNAL m_axi_rdata  : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL m_axi_rresp  : STD_LOGIC_VECTOR(1 DOWNTO 0);
  SIGNAL m_axi_rlast  : STD_LOGIC;
  SIGNAL m_axi_rvalid : STD_LOGIC;
  SIGNAL m_axi_rready : STD_LOGIC;

  -- 5. OSVVM records --
  SIGNAL AxiBus : Axi4RecType(
    WriteAddress(
      Addr(AXI_ADDR_WIDTH - 1 DOWNTO 0),
      ID(AXI_ID_WIDTH - 1 DOWNTO 0),
      User(AXI_USER_WIDTH - 1 DOWNTO 0)
    ),
    WriteData(
      Data(AXI_DATA_WIDTH - 1 DOWNTO 0),
      Strb(AXI_DATA_WIDTH/8 - 1 DOWNTO 0),
      User(AXI_USER_WIDTH - 1 DOWNTO 0),
      ID(AXI_ID_WIDTH - 1 DOWNTO 0)
    ),
    WriteResponse(
      ID(AXI_ID_WIDTH - 1 DOWNTO 0),
      User(AXI_USER_WIDTH - 1 DOWNTO 0)
    ),
    ReadAddress(
      Addr(AXI_ADDR_WIDTH - 1 DOWNTO 0),
      ID(AXI_ID_WIDTH - 1 DOWNTO 0),
      User(AXI_USER_WIDTH - 1 DOWNTO 0)
    ),
    ReadData(
      Data(AXI_DATA_WIDTH - 1 DOWNTO 0),
      ID(AXI_ID_WIDTH - 1 DOWNTO 0),
      User(AXI_USER_WIDTH - 1 DOWNTO 0)
    )
  );

  SIGNAL MemRec : AddressBusRecType(
    Address(AXI_ADDR_WIDTH - 1 DOWNTO 0),
    DataToModel(AXI_DATA_WIDTH - 1 DOWNTO 0),
    DataFromModel(AXI_DATA_WIDTH - 1 DOWNTO 0)
  );

  -- ======================================================================== --
  -- Helper procedures                                                        --
  -- ======================================================================== --

  PROCEDURE wait_cycles(SIGNAL clk_s : IN STD_ULOGIC; n : NATURAL) IS
  BEGIN
    FOR i IN 1 TO n LOOP
      WAIT UNTIL rising_edge(clk_s);
    END LOOP;
  END PROCEDURE;

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
    LOOP
      WAIT UNTIL rising_edge(clk_s);
      IF rsp.ack = '1' THEN
        data := rsp.data;
        err := rsp.err;
        RETURN;
      END IF;
    END LOOP;
  END PROCEDURE;

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
    LOOP
      WAIT UNTIL rising_edge(clk_s);
      IF rsp.ack = '1' THEN
        err := rsp.err;
        RETURN;
      END IF;
    END LOOP;
  END PROCEDURE;

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

BEGIN

  -- ======================================================================== --
  -- Clock generation                                                         --
  -- ======================================================================== --
  clk <= NOT clk AFTER CLK_PERIOD / 2;

  -- ======================================================================== --
  -- Component 1: Cache (DUT)                                                 --
  -- ======================================================================== --
  dut_cache : ENTITY neorv32.neorv32_cache_wb
    GENERIC MAP(
      NUM_BLOCKS => NUM_BLOCKS,
      BLOCK_SIZE => BLOCK_SIZE,
      UC_BEGIN   => UC_BEGIN,
      READ_ONLY  => false,
      BURSTS_EN  => BURSTS_EN
    )
    PORT MAP(
      clk_i      => clk,
      rstn_i     => rstn,
      host_req_i => host_req,
      host_rsp_o => host_rsp,
      bus_req_o  => cache_bus_req,
      bus_rsp_i  => cache_bus_rsp
    );

  -- ======================================================================== --
  -- Component 2: XBUS Gateway                                                --
  -- ======================================================================== --
  xbus_gateway : ENTITY neorv32.neorv32_xbus
    GENERIC MAP(
      REGSTAGE_EN => REGSTAGE_EN
    )
    PORT MAP(
      clk_i      => clk,
      rstn_i     => rstn,
      bus_term_i => '0',
      bus_req_i  => cache_bus_req,
      bus_rsp_o  => cache_bus_rsp,
      xbus_adr_o => xbus_adr,
      xbus_dat_i => xbus_dat_i,
      xbus_dat_o => xbus_dat_o,
      xbus_cti_o => xbus_cti,
      xbus_tag_o => xbus_tag,
      xbus_we_o  => xbus_we,
      xbus_sel_o => xbus_sel,
      xbus_stb_o => xbus_stb,
      xbus_cyc_o => xbus_cyc,
      xbus_ack_i => xbus_ack,
      xbus_err_i => xbus_err
    );

  -- ======================================================================== --
  -- Component 3: AXI Bridge                                                  --
  -- ======================================================================== --
  axi_bridge : ENTITY neorv32.xbus2axi4_bridge
    GENERIC MAP(
      BURST_EN  => BURSTS_EN,
      BURST_LEN => BURST_LEN
    )
    PORT MAP(
      clk           => STD_LOGIC(clk),
      resetn        => STD_LOGIC(rstn),
      -- XBUS side --
      xbus_adr_i    => xbus_adr,
      xbus_dat_i    => xbus_dat_o,
      xbus_cti_i    => xbus_cti,
      xbus_tag_i    => xbus_tag,
      xbus_we_i     => xbus_we,
      xbus_sel_i    => xbus_sel,
      xbus_stb_i    => xbus_stb,
      xbus_dat_o    => xbus_dat_i,
      xbus_ack_o    => xbus_ack,
      xbus_err_o    => xbus_err,
      -- AXI4 Write Address Channel --
      m_axi_awaddr  => m_axi_awaddr,
      m_axi_awlen   => m_axi_awlen,
      m_axi_awsize  => m_axi_awsize,
      m_axi_awburst => m_axi_awburst,
      m_axi_awcache => m_axi_awcache,
      m_axi_awprot  => m_axi_awprot,
      m_axi_awvalid => m_axi_awvalid,
      m_axi_awready => m_axi_awready,
      -- AXI4 Write Data Channel --
      m_axi_wdata   => m_axi_wdata,
      m_axi_wstrb   => m_axi_wstrb,
      m_axi_wlast   => m_axi_wlast,
      m_axi_wvalid  => m_axi_wvalid,
      m_axi_wready  => m_axi_wready,
      -- AXI4 Read Address Channel --
      m_axi_araddr  => m_axi_araddr,
      m_axi_arlen   => m_axi_arlen,
      m_axi_arsize  => m_axi_arsize,
      m_axi_arburst => m_axi_arburst,
      m_axi_arcache => m_axi_arcache,
      m_axi_arprot  => m_axi_arprot,
      m_axi_arvalid => m_axi_arvalid,
      m_axi_arready => m_axi_arready,
      -- AXI4 Read Data Channel --
      m_axi_rdata   => m_axi_rdata,
      m_axi_rresp   => m_axi_rresp,
      m_axi_rlast   => m_axi_rlast,
      m_axi_rvalid  => m_axi_rvalid,
      m_axi_rready  => m_axi_rready,
      -- AXI4 Write Response Channel --
      m_axi_bresp   => m_axi_bresp,
      m_axi_bvalid  => m_axi_bvalid,
      m_axi_bready  => m_axi_bready
    );

  -- ======================================================================== --
  -- AXI4 flat signal ↔ OSVVM record binding                                  --
  -- ======================================================================== --

  -- Read Address Channel --
  AxiBus.ReadAddress.Addr   <= m_axi_araddr;
  AxiBus.ReadAddress.Len    <= m_axi_arlen;
  AxiBus.ReadAddress.Size   <= m_axi_arsize;
  AxiBus.ReadAddress.Burst  <= m_axi_arburst;
  AxiBus.ReadAddress.ID     <= (OTHERS => '0');
  AxiBus.ReadAddress.Lock   <= '0';
  AxiBus.ReadAddress.Cache  <= m_axi_arcache;
  AxiBus.ReadAddress.Prot   <= m_axi_arprot;
  AxiBus.ReadAddress.QOS    <= (OTHERS => '0');
  AxiBus.ReadAddress.Region <= (OTHERS => '0');
  AxiBus.ReadAddress.User   <= (OTHERS => '0');
  AxiBus.ReadAddress.Valid  <= m_axi_arvalid;
  m_axi_arready <= AxiBus.ReadAddress.Ready;

  -- Read Data Channel --
  m_axi_rdata  <= AxiBus.ReadData.Data;
  m_axi_rresp  <= AxiBus.ReadData.Resp;
  m_axi_rlast  <= AxiBus.ReadData.Last;
  m_axi_rvalid <= AxiBus.ReadData.Valid;
  AxiBus.ReadData.Ready <= m_axi_rready;

  -- Write Address Channel --
  AxiBus.WriteAddress.Addr   <= m_axi_awaddr;
  AxiBus.WriteAddress.Len    <= m_axi_awlen;
  AxiBus.WriteAddress.Size   <= m_axi_awsize;
  AxiBus.WriteAddress.Burst  <= m_axi_awburst;
  AxiBus.WriteAddress.ID     <= (OTHERS => '0');
  AxiBus.WriteAddress.Lock   <= '0';
  AxiBus.WriteAddress.Cache  <= m_axi_awcache;
  AxiBus.WriteAddress.Prot   <= m_axi_awprot;
  AxiBus.WriteAddress.QOS    <= (OTHERS => '0');
  AxiBus.WriteAddress.Region <= (OTHERS => '0');
  AxiBus.WriteAddress.User   <= (OTHERS => '0');
  AxiBus.WriteAddress.Valid  <= m_axi_awvalid;
  m_axi_awready <= AxiBus.WriteAddress.Ready;

  -- Write Data Channel --
  AxiBus.WriteData.Data  <= m_axi_wdata;
  AxiBus.WriteData.Strb  <= m_axi_wstrb;
  AxiBus.WriteData.Last  <= m_axi_wlast;
  AxiBus.WriteData.User  <= (OTHERS => '0');
  AxiBus.WriteData.ID    <= (OTHERS => '0');
  AxiBus.WriteData.Valid <= m_axi_wvalid;
  m_axi_wready <= AxiBus.WriteData.Ready;

  -- Write Response Channel --
  m_axi_bresp  <= AxiBus.WriteResponse.Resp;
  m_axi_bvalid <= AxiBus.WriteResponse.Valid;
  AxiBus.WriteResponse.Ready <= m_axi_bready;

  -- ======================================================================== --
  -- OSVVM AXI4 Memory Subordinate                                            --
  -- ======================================================================== --
  axi_mem : ENTITY osvvm_axi4.Axi4Memory
    PORT MAP(
      Clk      => STD_LOGIC(clk),
      nReset   => STD_LOGIC(rstn),
      AxiBus   => AxiBus,
      TransRec => MemRec
    );

  -- ======================================================================== --
  -- OSVVM initialization                                                     --
  -- ======================================================================== --
  OsvvmInit : PROCESS
  BEGIN
    SetAlertLogName("tb_cache_chain");
    WAIT;
  END PROCESS;

  -- ======================================================================== --
  -- Test sequencer                                                           --
  -- ======================================================================== --
  TestSequencer : PROCESS
    VARIABLE rdata    : STD_ULOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE err      : STD_ULOGIC;
    VARIABLE mem_data : STD_LOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE test_addr : unsigned(31 DOWNTO 0);
  BEGIN
    test_runner_setup(runner, runner_cfg);
    WAIT FOR 0 ns; -- let OSVVM initialize

    WHILE test_suite LOOP
      -- Reset sequence --
      rstn <= '0';
      host_req <= req_terminate_c;
      WAIT FOR CLK_PERIOD * 10;
      rstn <= '1';
      WAIT FOR CLK_PERIOD * 5;

      -- ================================================================
      -- Group A: Baseline (verify the chain works at all)
      -- ================================================================

      -- ----------------------------------------------------------------
      -- A1. read_miss_then_hit
      -- Seed one word, read (miss → fill), read again (hit).
      -- ----------------------------------------------------------------
      IF run("read_miss_then_hit") THEN
        test_addr := BASE_ADDR;
        Write(MemRec, STD_LOGIC_VECTOR(test_addr), x"AABBCCDD");

        -- First read: miss → burst fill
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read miss error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"AABBCCDD"), "read miss data mismatch");

        -- Second read: hit
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read hit error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"AABBCCDD"), "read hit data mismatch");
      END IF;

      -- ----------------------------------------------------------------
      -- A2. read_block_words
      -- Seed 4 words in one block. Read word 0 (miss, fills entire
      -- line). Read words 1-3 (all hits).
      -- ----------------------------------------------------------------
      IF run("read_block_words") THEN
        test_addr := BASE_ADDR;
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          Write(MemRec, STD_LOGIC_VECTOR(test_addr + i * 4),
                STD_LOGIC_VECTOR(to_unsigned(16#F000# + i, 32)));
        END LOOP;

        -- Word 0: miss → fills entire block
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "word 0 read error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"0000F000"), "word 0 data mismatch");

        -- Words 1-3: all hits
        FOR i IN 1 TO WORDS_PER_BLK - 1 LOOP
          cache_read(clk, host_req, host_rsp,
                     STD_ULOGIC_VECTOR(test_addr + i * 4), rdata, err);
          check_equal(err, '0', "word " & INTEGER'image(i) & " read error");
          check_equal(rdata,
                      STD_ULOGIC_VECTOR(to_unsigned(16#F000# + i, 32)),
                      "word " & INTEGER'image(i) & " data mismatch");
        END LOOP;
      END IF;

      -- ----------------------------------------------------------------
      -- A3. write_miss
      -- Write to uncached address (S_DIRECT_REQ → single AXI write).
      -- Inspect OSVVM memory. Then read (miss, fills from written data).
      -- ----------------------------------------------------------------
      IF run("write_miss") THEN
        test_addr := BASE_ADDR;

        -- Write miss → direct write to memory
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"CAFEBABE"), "1111", err);
        check_equal(err, '0', "write miss error");

        -- Inspect OSVVM memory
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"CAFEBABE"),
                    "write miss: data should be in memory");

        -- Read triggers miss → fill from written data
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read after write miss error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"CAFEBABE"), "read after write miss data mismatch");
      END IF;

      -- ================================================================
      -- Group B: Writeback tests (core bug reproduction)
      -- ================================================================

      -- ----------------------------------------------------------------
      -- B1. dirty_eviction
      -- The primary bug reproduction test. Fill addr_A, dirty it,
      -- read addr_B at same index → dirty eviction (write burst) +
      -- fill (read burst). Verify writeback reached OSVVM memory.
      -- ----------------------------------------------------------------
      IF run("dirty_eviction") THEN
        test_addr := BASE_ADDR;
        -- Seed addr_A and addr_B (same index, different tags)
        Write(MemRec, STD_LOGIC_VECTOR(test_addr), x"AABBCCDD");
        Write(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE), x"11223344");

        -- Read addr_A → miss, fills clean
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read A error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"AABBCCDD"), "read A data mismatch");

        -- Write 0xDEADBEEF to addr_A → hit, marks dirty
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"DEADBEEF"), "1111", err);
        check_equal(err, '0', "write A error");

        -- Read addr_A → hit, returns dirty data
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read A after write error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"DEADBEEF"), "read A after write data mismatch");

        -- Inspect OSVVM: addr_A should still have old data (not written back yet)
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"AABBCCDD"),
                    "memory should have old data before eviction");

        -- Read addr_B → miss, triggers: dirty eviction of A + fill of B
        cache_read(clk, host_req, host_rsp,
                   STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE), rdata, err);
        check_equal(err, '0', "read B error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"11223344"), "read B data mismatch");

        -- Inspect OSVVM: addr_A should now have 0xDEADBEEF (written back)
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"DEADBEEF"),
                    "memory should have written-back data after eviction");

        -- Read addr_A → miss (evicted), refills, returns written-back data
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read A after eviction error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"DEADBEEF"),
                    "read A after eviction should return written-back data");
      END IF;

      -- ----------------------------------------------------------------
      -- B2. dirty_fence_flush
      -- Fill all 4 lines, write to each (all dirty), fence, read
      -- all back. Verify fence flushed all dirty data to OSVVM memory.
      -- ----------------------------------------------------------------
      IF run("dirty_fence_flush") THEN
        -- Seed and fill all cache lines via reads
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          Write(MemRec, STD_LOGIC_VECTOR(test_addr),
                STD_LOGIC_VECTOR(to_unsigned(16#A000# + i, 32)));
          cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
          check_equal(err, '0', "fill read error line " & INTEGER'image(i));
        END LOOP;

        -- Write new data to each line → all marked dirty
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                      STD_ULOGIC_VECTOR(to_unsigned(16#D000# + i, 32)), "1111", err);
          check_equal(err, '0', "write error line " & INTEGER'image(i));
        END LOOP;

        -- Fence → triggers write bursts for all dirty lines + invalidate
        cache_fence(clk, host_req);

        -- Re-read all: miss (invalidated), refills from memory with written-back data
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
          check_equal(err, '0', "post-fence read error line " & INTEGER'image(i));
          check_equal(rdata,
                      STD_ULOGIC_VECTOR(to_unsigned(16#D000# + i, 32)),
                      "post-fence data mismatch line " & INTEGER'image(i));
        END LOOP;

        -- Inspect OSVVM: all lines should have written-back data
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
          check_equal(STD_ULOGIC_VECTOR(mem_data),
                      STD_ULOGIC_VECTOR(to_unsigned(16#D000# + i, 32)),
                      "memory should have written-back data line " & INTEGER'image(i));
        END LOOP;
      END IF;

      -- ----------------------------------------------------------------
      -- B3. partial_dirty_flush
      -- Fill all 4 lines, write only even-indexed (0, 2). Fence.
      -- Inspect: even indices have new data, odd have original seed.
      -- ----------------------------------------------------------------
      IF run("partial_dirty_flush") THEN
        -- Seed and fill all cache lines
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          Write(MemRec, STD_LOGIC_VECTOR(test_addr),
                STD_LOGIC_VECTOR(to_unsigned(16#A000# + i, 32)));
          cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
          check_equal(err, '0', "fill read error line " & INTEGER'image(i));
        END LOOP;

        -- Write new data to even-indexed lines only
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          IF (i MOD 2) = 0 THEN
            test_addr := BASE_ADDR + i * BLOCK_STRIDE;
            cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                        STD_ULOGIC_VECTOR(to_unsigned(16#D000# + i, 32)), "1111", err);
            check_equal(err, '0', "write error line " & INTEGER'image(i));
          END IF;
        END LOOP;

        -- Fence → flushes dirty (even) lines, skips clean (odd) lines
        cache_fence(clk, host_req);

        -- Read all lines (forces fence to complete, all should be misses)
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
          check_equal(err, '0', "post-fence read error line " & INTEGER'image(i));
        END LOOP;

        -- Inspect memory: even lines have new data, odd lines have original
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
          IF (i MOD 2) = 0 THEN
            check_equal(STD_ULOGIC_VECTOR(mem_data),
                        STD_ULOGIC_VECTOR(to_unsigned(16#D000# + i, 32)),
                        "even line " & INTEGER'image(i) & " should have new data");
          ELSE
            check_equal(STD_ULOGIC_VECTOR(mem_data),
                        STD_ULOGIC_VECTOR(to_unsigned(16#A000# + i, 32)),
                        "odd line " & INTEGER'image(i) & " should have original data");
          END IF;
        END LOOP;
      END IF;

      -- ----------------------------------------------------------------
      -- B4. byte_write_dirty
      -- Seed 0x12345678, read (fill), partial write ben="0100"
      -- (byte 2 → 0xFF). Cache holds 0x12FF5678. Evict via
      -- conflicting read. Inspect: OSVVM has merged 0x12FF5678.
      -- ----------------------------------------------------------------
      IF run("byte_write_dirty") THEN
        test_addr := BASE_ADDR;

        -- Seed word and eviction address
        Write(MemRec, STD_LOGIC_VECTOR(test_addr), x"12345678");
        Write(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE), x"00000000");

        -- Read → miss, fills cache line
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "fill read error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"12345678"), "fill data mismatch");

        -- Partial write: only byte 2 (bits 23:16) updated
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"FFFFFFFF"), "0100", err);
        check_equal(err, '0', "partial write error");

        -- Read back → hit, should show merged data
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read after partial write error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"12FF5678"), "only byte 2 should have changed");

        -- Memory should still have original data
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"12345678"),
                    "memory should have old data before eviction");

        -- Trigger eviction: read different tag at same index
        cache_read(clk, host_req, host_rsp,
                   STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE), rdata, err);
        check_equal(err, '0', "eviction read error");

        -- Memory should now have merged data
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"12FF5678"),
                    "memory should have merged data after writeback");
      END IF;

      -- ----------------------------------------------------------------
      -- B5. multi_word_writeback
      -- Seed 4 distinct words in one block. Read (fills entire line).
      -- Write only word 0 with 0xDEADBEEF (marks line dirty). Evict.
      -- Inspect all 4 words: word 0 = 0xDEADBEEF, words 1-3 = seed.
      -- ----------------------------------------------------------------
      IF run("multi_word_writeback") THEN
        test_addr := BASE_ADDR;

        -- Seed all words of block with distinct values
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          Write(MemRec, STD_LOGIC_VECTOR(test_addr + i * 4),
                STD_LOGIC_VECTOR(to_unsigned(16#F000# + i, 32)));
        END LOOP;

        -- Seed eviction address
        Write(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE), x"00000000");

        -- Read word 0 → miss, fills entire block
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "fill read error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"0000F000"), "fill word 0 mismatch");

        -- Write ONLY word 0 → marks line dirty
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"DEADBEEF"), "1111", err);
        check_equal(err, '0', "write error");

        -- Trigger eviction → writeback entire line
        cache_read(clk, host_req, host_rsp,
                   STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE), rdata, err);
        check_equal(err, '0', "eviction read error");

        -- Inspect all words: word 0 new, others original
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          Read(MemRec, STD_LOGIC_VECTOR(test_addr + i * 4), mem_data);
          IF i = 0 THEN
            check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"DEADBEEF"),
                        "word 0 should have new value");
          ELSE
            check_equal(STD_ULOGIC_VECTOR(mem_data),
                        STD_ULOGIC_VECTOR(to_unsigned(16#F000# + i, 32)),
                        "word " & INTEGER'image(i) & " should have original fill value");
          END IF;
        END LOOP;
      END IF;

      -- ================================================================
      -- Group C: Integration-specific tests
      -- ================================================================

      -- ----------------------------------------------------------------
      -- C1. evict_then_read_back
      -- Seed 4-word blocks at addr_A and addr_B. Fill addr_A, write
      -- all 4 words. Read addr_B word 0 → dirty eviction (write burst)
      -- immediately followed by fill (read burst). Verify both.
      -- ----------------------------------------------------------------
      IF run("evict_then_read_back") THEN
        test_addr := BASE_ADDR;

        -- Seed all words of block A with distinct values
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          Write(MemRec, STD_LOGIC_VECTOR(test_addr + i * 4),
                STD_LOGIC_VECTOR(to_unsigned(16#AA00# + i, 32)));
        END LOOP;

        -- Seed all words of block B
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          Write(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE + i * 4),
                STD_LOGIC_VECTOR(to_unsigned(16#BB00# + i, 32)));
        END LOOP;

        -- Fill addr_A via read
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "fill A error");

        -- Write all 4 words of addr_A with new values → dirty
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          cache_write(clk, host_req, host_rsp,
                      STD_ULOGIC_VECTOR(test_addr + i * 4),
                      STD_ULOGIC_VECTOR(to_unsigned(16#CC00# + i, 32)), "1111", err);
          check_equal(err, '0', "write A word " & INTEGER'image(i) & " error");
        END LOOP;

        -- Read addr_B word 0 → eviction of A (write burst) + fill B (read burst)
        cache_read(clk, host_req, host_rsp,
                   STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE), rdata, err);
        check_equal(err, '0', "read B error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"0000BB00"), "read B word 0 data mismatch");

        -- Verify addr_A writeback: all 4 words in OSVVM memory
        FOR i IN 0 TO WORDS_PER_BLK - 1 LOOP
          Read(MemRec, STD_LOGIC_VECTOR(test_addr + i * 4), mem_data);
          check_equal(STD_ULOGIC_VECTOR(mem_data),
                      STD_ULOGIC_VECTOR(to_unsigned(16#CC00# + i, 32)),
                      "writeback A word " & INTEGER'image(i) & " mismatch");
        END LOOP;

        -- Verify addr_B fill: read remaining words (all hits)
        FOR i IN 1 TO WORDS_PER_BLK - 1 LOOP
          cache_read(clk, host_req, host_rsp,
                     STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE + i * 4), rdata, err);
          check_equal(err, '0', "read B word " & INTEGER'image(i) & " error");
          check_equal(rdata,
                      STD_ULOGIC_VECTOR(to_unsigned(16#BB00# + i, 32)),
                      "read B word " & INTEGER'image(i) & " data mismatch");
        END LOOP;
      END IF;

      -- ----------------------------------------------------------------
      -- C2. rapid_eviction_chain
      -- 3 addresses at same index, different tags. Sequence:
      -- fill A, dirty A, read B (evict A), dirty B, read C (evict B),
      -- dirty C, read A (evict C). Inspect all 3 addresses.
      -- ----------------------------------------------------------------
      IF run("rapid_eviction_chain") THEN
        test_addr := BASE_ADDR;

        -- Seed A, B, C
        Write(MemRec, STD_LOGIC_VECTOR(test_addr),                     x"000000AA");
        Write(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE),      x"000000BB");
        Write(MemRec, STD_LOGIC_VECTOR(test_addr + 2 * EVICT_STRIDE),  x"000000CC");

        -- Fill A, dirty A
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "fill A error");
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"DEAD000A"), "1111", err);
        check_equal(err, '0', "dirty A error");

        -- Read B → evict A, fill B
        cache_read(clk, host_req, host_rsp,
                   STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE), rdata, err);
        check_equal(err, '0', "read B error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"000000BB"), "read B data mismatch");

        -- Dirty B
        cache_write(clk, host_req, host_rsp,
                    STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE),
                    STD_ULOGIC_VECTOR'(x"DEAD000B"), "1111", err);
        check_equal(err, '0', "dirty B error");

        -- Read C → evict B, fill C
        cache_read(clk, host_req, host_rsp,
                   STD_ULOGIC_VECTOR(test_addr + 2 * EVICT_STRIDE), rdata, err);
        check_equal(err, '0', "read C error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"000000CC"), "read C data mismatch");

        -- Dirty C
        cache_write(clk, host_req, host_rsp,
                    STD_ULOGIC_VECTOR(test_addr + 2 * EVICT_STRIDE),
                    STD_ULOGIC_VECTOR'(x"DEAD000C"), "1111", err);
        check_equal(err, '0', "dirty C error");

        -- Read A → evict C, fill A
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read A after chain error");

        -- Inspect all 3 addresses in OSVVM
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"DEAD000A"),
                    "A should have written-back data");
        Read(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"DEAD000B"),
                    "B should have written-back data");
        Read(MemRec, STD_LOGIC_VECTOR(test_addr + 2 * EVICT_STRIDE), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"DEAD000C"),
                    "C should have written-back data");
      END IF;

      -- ----------------------------------------------------------------
      -- C3. dirty_all_then_sequential_evict
      -- Fill all 4 lines, make all dirty. Then for each index, read
      -- from conflicting tag (evict + fill). Verify all 4 writebacks
      -- reached OSVVM memory.
      -- ----------------------------------------------------------------
      IF run("dirty_all_then_sequential_evict") THEN
        -- Seed and fill all lines, then dirty each
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          Write(MemRec, STD_LOGIC_VECTOR(test_addr),
                STD_LOGIC_VECTOR(to_unsigned(16#A000# + i, 32)));
          -- Seed conflicting address for later eviction
          Write(MemRec, STD_LOGIC_VECTOR(test_addr + EVICT_STRIDE),
                STD_LOGIC_VECTOR(to_unsigned(16#B000# + i, 32)));
          cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
          check_equal(err, '0', "fill error line " & INTEGER'image(i));
          cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                      STD_ULOGIC_VECTOR(to_unsigned(16#E000# + i, 32)), "1111", err);
          check_equal(err, '0', "dirty error line " & INTEGER'image(i));
        END LOOP;

        -- Evict each line by reading conflicting tag
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          cache_read(clk, host_req, host_rsp,
                     STD_ULOGIC_VECTOR(test_addr + EVICT_STRIDE), rdata, err);
          check_equal(err, '0', "eviction read error line " & INTEGER'image(i));
          check_equal(rdata,
                      STD_ULOGIC_VECTOR(to_unsigned(16#B000# + i, 32)),
                      "eviction fill data mismatch line " & INTEGER'image(i));
        END LOOP;

        -- Verify all writebacks in OSVVM memory
        FOR i IN 0 TO NUM_BLOCKS - 1 LOOP
          test_addr := BASE_ADDR + i * BLOCK_STRIDE;
          Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
          check_equal(STD_ULOGIC_VECTOR(mem_data),
                      STD_ULOGIC_VECTOR(to_unsigned(16#E000# + i, 32)),
                      "writeback data mismatch line " & INTEGER'image(i));
        END LOOP;
      END IF;

      -- ----------------------------------------------------------------
      -- C4. fence_during_dirty
      -- Fill + dirty one line. Drive fence and immediately issue
      -- cache_read. Cache FSM prioritizes fence. Flush completes
      -- (write burst), cache invalidated, then read executes
      -- (miss → fill). Verify data correctness.
      -- ----------------------------------------------------------------
      IF run("fence_during_dirty") THEN
        test_addr := BASE_ADDR;
        Write(MemRec, STD_LOGIC_VECTOR(test_addr), x"AABBCCDD");

        -- Fill and dirty
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "fill error");
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"DEADBEEF"), "1111", err);
        check_equal(err, '0', "dirty error");

        -- Drive fence one cycle before stb so buf_syn is registered first
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

        -- Wait for ack
        LOOP
          WAIT UNTIL rising_edge(clk);
          IF host_rsp.ack = '1' THEN
            rdata := host_rsp.data;
            err := host_rsp.err;
            EXIT;
          END IF;
        END LOOP;

        check_equal(err, '0', "read after fence error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"DEADBEEF"),
                    "read after fence should return written-back data");

        -- Verify flush wrote dirty data to memory
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"DEADBEEF"),
                    "fence should have flushed dirty data to memory");
      END IF;

      -- ----------------------------------------------------------------
      -- C5. single_write_no_burst
      -- Write to address not in cache (write miss → S_DIRECT_REQ →
      -- single AXI write, no burst). Inspect OSVVM. Then read (fills
      -- from written data). Verifies bridge handles single writes
      -- correctly alongside burst logic.
      -- ----------------------------------------------------------------
      IF run("single_write_no_burst") THEN
        test_addr := BASE_ADDR;

        -- Write miss → direct single write to memory
        cache_write(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr),
                    STD_ULOGIC_VECTOR'(x"CAFED00D"), "1111", err);
        check_equal(err, '0', "single write error");

        -- Inspect OSVVM memory
        Read(MemRec, STD_LOGIC_VECTOR(test_addr), mem_data);
        check_equal(STD_ULOGIC_VECTOR(mem_data), STD_ULOGIC_VECTOR'(x"CAFED00D"),
                    "single write: data should be in memory");

        -- Read triggers miss → fill from written data
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read after single write error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"CAFED00D"),
                    "read after single write data mismatch");

        -- Read again → hit (line now cached)
        cache_read(clk, host_req, host_rsp, STD_ULOGIC_VECTOR(test_addr), rdata, err);
        check_equal(err, '0', "read hit error");
        check_equal(rdata, STD_ULOGIC_VECTOR'(x"CAFED00D"), "read hit data mismatch");
      END IF;

    END LOOP;

    test_runner_cleanup(runner);
  END PROCESS;

  test_runner_watchdog(runner, 10 ms);

END ARCHITECTURE;
