-- ================================================================================ --
-- NEORV32 SoC - XBUS to AXI4-Compatible Bridge                                     --
-- -------------------------------------------------------------------------------- --
-- This bridge supports single read/write transfers and read/write bursts.          --
-- -------------------------------------------------------------------------------- --
-- The NEORV32 RISC-V Processor - https://github.com/stnolting/neorv32              --
-- Copyright (c) NEORV32 contributors.                                              --
-- Copyright (c) 2020 - 2026 Stephan Nolting. All rights reserved.                  --
-- Licensed under the BSD-3-Clause license, see LICENSE for details.                --
-- SPDX-License-Identifier: BSD-3-Clause                                            --
-- ================================================================================ --

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity xbus2axi4_bridge is
  generic (
    BURST_EN  : boolean; -- enable burst transfers
    BURST_LEN : natural range 4 to 1024 -- bytes per burst, has to be a multiple of 4
  );
  port (
    -- Global control --
    clk           : in  std_logic;
    resetn        : in  std_logic;
    -- XBUS device interface --
    xbus_adr_i    : in  std_ulogic_vector(31 downto 0);
    xbus_dat_i    : in  std_ulogic_vector(31 downto 0);
    xbus_cti_i    : in  std_ulogic_vector(2 downto 0);
    xbus_tag_i    : in  std_ulogic_vector(2 downto 0);
    xbus_we_i     : in  std_ulogic;
    xbus_sel_i    : in  std_ulogic_vector(3 downto 0);
    xbus_stb_i    : in  std_ulogic;
    xbus_dat_o    : out std_ulogic_vector(31 downto 0);
    xbus_ack_o    : out std_ulogic;
    xbus_err_o    : out std_ulogic;
    -- AXI4 host write address channel --
    m_axi_awaddr  : out std_logic_vector(31 downto 0);
    m_axi_awlen   : out std_logic_vector(7 downto 0);
    m_axi_awsize  : out std_logic_vector(2 downto 0);
    m_axi_awburst : out std_logic_vector(1 downto 0);
    m_axi_awcache : out std_logic_vector(3 downto 0);
    m_axi_awprot  : out std_logic_vector(2 downto 0);
    m_axi_awvalid : out std_logic;
    m_axi_awready : in  std_logic;
    -- AXI4 host write data channel --
    m_axi_wdata   : out std_logic_vector(31 downto 0);
    m_axi_wstrb   : out std_logic_vector(3 downto 0);
    m_axi_wlast   : out std_logic;
    m_axi_wvalid  : out std_logic;
    m_axi_wready  : in  std_logic;
    -- AXI4 host read address channel --
    m_axi_araddr  : out std_logic_vector(31 downto 0);
    m_axi_arlen   : out std_logic_vector(7 downto 0);
    m_axi_arsize  : out std_logic_vector(2 downto 0);
    m_axi_arburst : out std_logic_vector(1 downto 0);
    m_axi_arcache : out std_logic_vector(3 downto 0);
    m_axi_arprot  : out std_logic_vector(2 downto 0);
    m_axi_arvalid : out std_logic;
    m_axi_arready : in  std_logic;
    -- AXI4 host read data channel --
    m_axi_rdata   : in  std_logic_vector(31 downto 0);
    m_axi_rresp   : in  std_logic_vector(1 downto 0);
    m_axi_rlast   : in  std_logic;
    m_axi_rvalid  : in  std_logic;
    m_axi_rready  : out std_logic;
    -- AXI4 host write response channel --
    m_axi_bresp   : in  std_logic_vector(1 downto 0);
    m_axi_bvalid  : in  std_logic;
    m_axi_bready  : out std_logic
  );
end entity;

architecture xbus2axi4_bridge_rtl of xbus2axi4_bridge is

  signal state : std_ulogic_vector(1 downto 0);
  signal arvalid, awvalid, wvalid, xbus_rd_ack, xbus_rd_err, xbus_wr_ack, xbus_wr_err : std_ulogic;
  constant blen_c : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned((BURST_LEN/4)-1, 8));
  signal wr_burst_addr : std_logic_vector(31 downto 0);
  signal wr_burst_data : std_logic_vector(31 downto 0);
  signal wr_burst_strb : std_logic_vector(3 downto 0);
  signal wr_beat_cnt   : unsigned(7 downto 0);
  signal wr_all_sent   : std_ulogic;
  signal wr_burst_ack  : std_ulogic;
  signal wr_burst_err  : std_ulogic;

begin

  -- AXI arbiter --
  arbiter: process(resetn, clk)
  begin
    if (resetn = '0') then
      arvalid       <= '0';
      awvalid       <= '0';
      wvalid        <= '0';
      state         <= (others => '0');
      wr_burst_addr <= (others => '0');
      wr_burst_data <= (others => '0');
      wr_burst_strb <= (others => '0');
      wr_beat_cnt   <= (others => '0');
      wr_all_sent   <= '0';
    elsif rising_edge(clk) then
      -- AXI handshake --
      arvalid <= arvalid and std_ulogic(not m_axi_arready);
      awvalid <= awvalid and std_ulogic(not m_axi_awready);
      wvalid  <= wvalid  and std_ulogic(not m_axi_wready);
      -- state machine --
      case state is

        when "00" => -- idle; wait for access request
        -- ------------------------------------------------------------
          arvalid <= '0';
          awvalid <= '0';
          wvalid  <= '0';
          if (xbus_stb_i = '1') then -- access request
            if BURST_EN and (xbus_cti_i = "010") then -- incrementing address burst access
              if (xbus_we_i = '0') then -- read burst
                arvalid <= '1';
                state   <= "10";
              else -- write burst
                awvalid       <= '1';
                wvalid        <= '1';
                wr_burst_addr <= std_logic_vector(xbus_adr_i);
                wr_burst_data <= std_logic_vector(xbus_dat_i);
                wr_burst_strb <= std_logic_vector(xbus_sel_i);
                wr_beat_cnt   <= (others => '0');
                wr_all_sent   <= '0';
                state         <= "11";
              end if;
            else -- single access (read/write)
              arvalid <= not xbus_we_i;
              awvalid <= xbus_we_i;
              wvalid  <= xbus_we_i;
              state   <= "01";
            end if;
          end if;

        when "01" => -- single read/write transfer in progress
        -- ------------------------------------------------------------
          if (m_axi_rvalid = '1') or (m_axi_bvalid = '1') then
            state <= (others => '0');
          end if;

        when "10" => -- burst read transfer in progress
        -- ------------------------------------------------------------
          if (BURST_EN = false) or (xbus_cti_i = "000") then -- burst completed by host
            state <= (others => '0');
          end if;

        when "11" => -- burst write transfer in progress
        -- ------------------------------------------------------------
          if (wr_all_sent = '0') then
            -- latch next beat data when cache presents it and previous beat was accepted
            if (xbus_stb_i = '1') and (wvalid = '0') then
              wr_burst_data <= std_logic_vector(xbus_dat_i);
              wr_burst_strb <= std_logic_vector(xbus_sel_i);
              wvalid        <= '1';
              wr_beat_cnt   <= wr_beat_cnt + 1;
            end if;
            -- detect last beat accepted by AXI slave
            if (wr_beat_cnt = unsigned(blen_c)) and (wvalid = '1') and (m_axi_wready = '1') then
              wr_all_sent <= '1';
            end if;
          end if;
          -- wait for write response to complete transaction
          if (m_axi_bvalid = '1') then
            state <= (others => '0');
          end if;

        when others => -- undefined
        -- ------------------------------------------------------------
          state <= (others => '0');

      end case;
    end if;
  end process arbiter;

  -- AXI read address channel --
  m_axi_araddr  <= std_logic_vector(xbus_adr_i);
  m_axi_arlen   <= blen_c when BURST_EN and (state = "10") else (others => '0'); -- burst length
  m_axi_arsize  <= "010"; -- 4 bytes per transfer
  m_axi_arburst <= "01"; -- incrementing bursts only
  m_axi_arcache <= "0011"; -- recommended by Vivado
  m_axi_arprot  <= std_logic_vector(xbus_tag_i);
  m_axi_arvalid <= std_logic(arvalid);

  -- AXI read data channel --
  m_axi_rready  <= '1'; -- always ready for read response
  xbus_rd_ack   <= '1' when (m_axi_rvalid = '1') and (m_axi_rresp(1) = '0') else '0'; -- OKAY(00)/EXOKAY(01)
  xbus_rd_err   <= '1' when (m_axi_rvalid = '1') and (m_axi_rresp(1) = '1') else '0'; -- SLVERR(10)/DECERR(11)
  xbus_dat_o    <= std_ulogic_vector(m_axi_rdata);

  -- AXI write address channel --
  m_axi_awaddr  <= wr_burst_addr when (state = "11") else std_logic_vector(xbus_adr_i);
  m_axi_awlen   <= blen_c when (state = "11") else (others => '0');
  m_axi_awsize  <= "010"; -- 4 bytes per transfer
  m_axi_awburst <= "01"; -- incrementing bursts only
  m_axi_awcache <= "0011"; -- recommended by Vivado
  m_axi_awprot  <= std_logic_vector(xbus_tag_i);
  m_axi_awvalid <= std_logic(awvalid);

  -- AXI write data channel --
  m_axi_wdata   <= wr_burst_data when (state = "11") else std_logic_vector(xbus_dat_i);
  m_axi_wstrb   <= wr_burst_strb when (state = "11") else std_logic_vector(xbus_sel_i);
  m_axi_wlast   <= '1' when (state /= "11") or (wr_beat_cnt = unsigned(blen_c)) else '0';
  m_axi_wvalid  <= std_logic(wvalid);

  -- AXI write response channel --
  m_axi_bready  <= '1'; -- always ready for write response
  xbus_wr_ack   <= '1' when (state = "01") and (m_axi_bvalid = '1') and (m_axi_bresp(1) = '0') else '0';
  xbus_wr_err   <= '1' when (state = "01") and (m_axi_bvalid = '1') and (m_axi_bresp(1) = '1') else '0';

  -- burst write ack/err --
  wr_burst_ack  <= '1' when (state = "11") and (wr_all_sent = '0') and (wvalid = '1') and (m_axi_wready = '1') and (wr_beat_cnt /= unsigned(blen_c)) else
                   '1' when (state = "11") and (m_axi_bvalid = '1') and (m_axi_bresp(1) = '0') else '0';
  wr_burst_err  <= '1' when (state = "11") and (m_axi_bvalid = '1') and (m_axi_bresp(1) = '1') else '0';

  -- XBUS response --
  xbus_ack_o    <= xbus_rd_ack or xbus_wr_ack or wr_burst_ack;
  xbus_err_o    <= xbus_rd_err or xbus_wr_err or wr_burst_err;

end architecture;
