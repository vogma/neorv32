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
    VARIABLE rdata : STD_ULOGIC_VECTOR(31 DOWNTO 0);
    VARIABLE err   : STD_ULOGIC;
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

      -- Test cases will be added here --
      -- Example structure:
      -- if run("test_name") then
      --   Write(MemRec, addr, data);       -- seed AXI memory
      --   cache_read(clk, host_req, host_rsp, addr, rdata, err);
      --   Read(MemRec, addr, readback);    -- inspect AXI memory
      --   check_equal(readback, expected);
      -- end if;

    END LOOP;

    test_runner_cleanup(runner);
  END PROCESS;

  test_runner_watchdog(runner, 10 ms);

END ARCHITECTURE;
