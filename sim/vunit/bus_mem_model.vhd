-- ================================================================================ --
-- Bus Memory Model for VUnit Cache Testbench                                       --
-- -------------------------------------------------------------------------------- --
-- Simple synchronous memory with bus_req_t/bus_rsp_t interface.                    --
-- Supports byte-granular writes, configurable latency, burst transfers,            --
-- and a direct inspection port for test assertions.                                --
-- ================================================================================ --

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;

entity bus_mem_model is
  generic (
    MEM_SIZE : natural := 4096; -- bytes
    LATENCY  : natural := 1    -- response latency in clock cycles
  );
  port (
    clk_i          : in  std_ulogic;
    rstn_i         : in  std_ulogic;
    -- bus interface --
    bus_req_i      : in  bus_req_t;
    bus_rsp_o      : out bus_rsp_t;
    -- error injection --
    force_err_i    : in  std_ulogic;
    -- direct inspection (bypasses cache) --
    inspect_addr_i : in  std_ulogic_vector(31 downto 0);
    inspect_data_o : out std_ulogic_vector(31 downto 0)
  );
end entity;

architecture bus_mem_model_rtl of bus_mem_model is

  type mem_t is array(0 to MEM_SIZE-1) of std_ulogic_vector(7 downto 0);
  signal mem : mem_t := (others => (others => '0'));

  -- latency pipeline --
  type pending_t is record
    valid : std_ulogic;
    data  : std_ulogic_vector(31 downto 0);
    err   : std_ulogic;
  end record;
  type pipe_t is array(0 to LATENCY) of pending_t;
  signal pipe : pipe_t := (others => (valid => '0', data => (others => '0'), err => '0'));

begin

  -- bus access --
  bus_proc: process(clk_i, rstn_i)
    variable addr_v : natural;
  begin
    if rstn_i = '0' then
      pipe <= (others => (valid => '0', data => (others => '0'), err => '0'));
    elsif rising_edge(clk_i) then
      -- shift pipeline --
      for i in LATENCY downto 1 loop
        pipe(i) <= pipe(i-1);
      end loop;
      -- default: nothing entering pipeline --
      pipe(0).valid <= '0';
      pipe(0).data  <= (others => '0');
      pipe(0).err   <= '0';

      if bus_req_i.stb = '1' then
        addr_v := to_integer(unsigned(bus_req_i.addr(index_size_f(MEM_SIZE)-1 downto 0)));
        -- ensure word-aligned base --
        addr_v := (addr_v / 4) * 4;

        if bus_req_i.rw = '1' then
          -- write --
          for b in 0 to 3 loop
            if bus_req_i.ben(b) = '1' and (addr_v + b) < MEM_SIZE then
              mem(addr_v + b) <= bus_req_i.data(b*8+7 downto b*8);
            end if;
          end loop;
          pipe(0).valid <= '1';
          pipe(0).data  <= (others => '0');
          pipe(0).err   <= force_err_i;
        else
          -- read --
          if (addr_v + 3) < MEM_SIZE then
            pipe(0).data(7  downto  0) <= mem(addr_v + 0);
            pipe(0).data(15 downto  8) <= mem(addr_v + 1);
            pipe(0).data(23 downto 16) <= mem(addr_v + 2);
            pipe(0).data(31 downto 24) <= mem(addr_v + 3);
          else
            pipe(0).data <= (others => '0');
          end if;
          pipe(0).valid <= '1';
          pipe(0).err   <= force_err_i;
        end if;
      elsif bus_req_i.fence = '1' then
        -- fence: immediate ack, no-op --
        pipe(0).valid <= '1';
        pipe(0).data  <= (others => '0');
        pipe(0).err   <= '0';
      end if;
    end if;
  end process bus_proc;

  -- output from end of pipeline --
  bus_rsp_o.ack  <= pipe(LATENCY).valid;
  bus_rsp_o.err  <= pipe(LATENCY).err;
  bus_rsp_o.data <= pipe(LATENCY).data;

  -- combinational inspection port --
  inspect_port: process(inspect_addr_i, mem)
    variable iaddr_v : natural;
  begin
    iaddr_v := to_integer(unsigned(inspect_addr_i(index_size_f(MEM_SIZE)-1 downto 0)));
    iaddr_v := (iaddr_v / 4) * 4;
    if (iaddr_v + 3) < MEM_SIZE then
      inspect_data_o(7  downto  0) <= mem(iaddr_v + 0);
      inspect_data_o(15 downto  8) <= mem(iaddr_v + 1);
      inspect_data_o(23 downto 16) <= mem(iaddr_v + 2);
      inspect_data_o(31 downto 24) <= mem(iaddr_v + 3);
    else
      inspect_data_o <= (others => '0');
    end if;
  end process inspect_port;

end architecture;
