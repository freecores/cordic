--
-- file: CordicPipe.vhd
-- author: Richard Herveille
-- rev. 1.0 initial release
-- rev. 1.1 March 19th, 2001. Richard Herveille. Changed function Delta, it is compatible with Xilinx WebPack software now
-- rev. 1.2 May   18th, 2001. Richard Herveille. Added documentation to function ATAN (by popular request).

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity CordicPipe is 
	generic(
		WIDTH 	: natural := 16;
		AWIDTH	: natural := 16;
		PIPEID	: natural := 1
	);
	port(
		clk		: in std_logic;
		ena		: in std_logic;

		Xi		: in std_logic_vector(WIDTH -1 downto 0); 
		Yi		: in std_logic_vector(WIDTH -1 downto 0);
		Zi		: in std_logic_vector(AWIDTH -1 downto 0);
		Xo		: out std_logic_vector(WIDTH -1 downto 0);
		Yo		: out std_logic_vector(WIDTH -1 downto 0);
		Zo		: out std_logic_vector(AWIDTH -1 downto 0)
	);
end entity CordicPipe;

architecture dataflow of CordicPipe is

	--
	-- functions
	--

	-- Function CATAN (constante arc-tangent).
	-- This is a lookup table containing pre-calculated arc-tangents.
	-- 'n' is the number of the pipe, returned is a 20bit arc-tangent value.
	-- The numbers are calculated as follows: Z(n) = atan(1/2^n)
	-- examples:
	-- 20bit values => 2^20 = 2pi(rad)
	--                 1(rad) = 2^20/2pi = 166886.053....
	-- n:1, atan(1/2) = 0.4636...(rad)
	--      0.4636... * 166886.053... = 77376.32(dec) = 12E40(hex)
	-- n:2, atan(1/4) = 0.2449...(rad)
	--      0.2449... * 166886.053... = 40883.52(dec) = 9FB3(hex)
	-- n:3, atan(1/8) = 0.1243...(rad)
	--      0.1243... * 166886.053... = 20753.11(dec) = 5111(hex)
	--
	function CATAN(n :natural) return integer is
	variable result	:integer;
	begin
		case n is
			when 1 => result := 16#012E40#;
			when 2 => result := 16#09FB4#;
			when 3 => result := 16#05111#;
			when 4 => result := 16#028B1#;
			when 5 => result := 16#0145D#;
			when 6 => result := 16#0A2F#;
			when 7 => result := 16#0518#;
			when 8 => result := 16#028C#;
			when 9 => result := 16#0146#;
			when 10 => result := 16#0A3#;
			when 11 => result := 16#051#;
			when 12 => result := 16#029#;
			when 13 => result := 16#014#;
			when 14 => result := 16#0A#;
			when 15 => result := 16#05#;
			when 16 => result := 16#03#;
			when 17 => result := 16#01#;
			when others => result := 16#0#;
		end case;
		return result;
	end CATAN;

	-- function Delta is actually an arithmatic shift right
	-- This strange construction is needed for compatibility with Xilinx WebPack
	function Delta(Arg : std_logic_vector; Cnt : natural) return std_logic_vector is
		variable tmp : std_logic_vector(Arg'range);
		constant lo : integer := Arg'high -cnt +1;
	begin
		for n in Arg'high downto lo loop
			tmp(n) := Arg(Arg'high);
		end loop;
		for n in Arg'high -cnt downto 0 loop
			tmp(n) := Arg(n +cnt);
		end loop;
		return tmp;
	end function Delta;

	function AddSub(dataa, datab : in std_logic_vector; add_sub : in std_logic) return std_logic_vector is
	begin
		if (add_sub = '1') then
			return unsigned(dataa) + unsigned(datab);
		else
			return unsigned(dataa) - unsigned(datab);
		end if;
	end;

	--
	--	ARCHITECTURE BODY
	--
	signal dX, Xresult	: std_logic_vector(WIDTH -1 downto 0);
	signal dY, Yresult	: std_logic_vector(WIDTH -1 downto 0);
	signal atan, Zresult	: std_logic_vector(AWIDTH -1 downto 0);

	signal Yneg, Ypos	: std_logic;
	
begin

	dX <= Delta(Xi, PIPEID);
	dY <= Delta(Yi, PIPEID);
	atan <= std_logic_vector(conv_unsigned( catan(PIPEID), AWIDTH) );

	-- generate adder structures
	Yneg <= Yi(WIDTH -1);
	Ypos <= not Yi(WIDTH -1);

	-- xadd
    Xresult <= AddSub(Xi, dY, YPos);

	-- yadd 
	Yresult <= AddSub(Yi, dX, Yneg);

	-- zadd
	Zresult <= AddSub(Zi, atan, Ypos);

	gen_regs: process(clk)
	begin
		if(clk'event and clk='1') then
			if (ena = '1') then
				Xo <= Xresult;
				Yo <= Yresult;
				Zo <= Zresult;
			end if;
		end if;
	end process;

end architecture dataflow;
