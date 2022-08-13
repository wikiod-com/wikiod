---
title: "Memories"
slug: "memories"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

This covers single port and dual port memories.

## Syntax
 - Memory type for constant width and depth.
   
       type MEMORY_TYPE is array (0 to DEPTH-1) of std_logic_vector(WIDTH-1 downto 0);
   
   Memory type for variable depth and constant width.
   
       type MEMORY_TYPE is array (natural range <>) of std_logic_vector(WIDTH-1 downto 0);

## Shift register
A shift register of generic length. With serial in and serial out.

    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    
    entity SHIFT_REG is
        generic(
            LENGTH: natural := 8
        );
        port(
            SHIFT_EN : in  std_logic;
            SO       : out std_logic;
            SI       : in  std_logic;
            clk      : in  std_logic;
            rst      : in  std_logic
        );
    end entity SHIFT_REG;
    
    architecture Behavioral of SHIFT_REG is
        signal reg : std_logic_vector(LENGTH-1 downto 0) := (others => '0');
    begin
        main_process : process(clk) is
        begin
            if rising_edge(clk) then
                if rst = '1' then
                    reg <= (others => '0');
                else
                    if SHIFT_EN = '1' then
                        --Shift 
                        reg <= reg(LENGTH-2 downto 0) & SI;
                    else
                        reg <= reg;
                    end if;
                end if;
            end if;
        end process main_process;
        
        SO <= reg(LENGTH-1);
    end architecture Behavioral;

For Parallel out,

    --In port
    DOUT: out std_logic_vector(LENGTH-1 downto 0);
    ----------------------------------------------
    --In architecture
    DOUT <= REG;

Shift register with direction control, parallel load, parallel out. (Using Variable instead of signal)

    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    
    entity SHIFT_REG_UNIVERSAL is
        generic(
            LENGTH : integer := 8
        );
        port(
            DIN  : in  std_logic_vector(LENGTH - 1 downto 0);
            DOUT : out std_logic_vector(LENGTH - 1 downto 0);
            MODE : in  std_logic_vector(1 downto 0);
            SI   : in  std_logic;
            clk  : in  std_logic;
            rst  : in  std_logic
        );
    end entity SHIFT_REG_UNIVERSAL;
    
    architecture RTL of SHIFT_REG_UNIVERSAL is
    begin
        main : process(clk, rst) is
            variable reg : std_logic_vector(LENGTH - 1 downto 0) := (others => '0');
        begin
            if rst = '1' then
                reg := (others => '0');
            elsif rising_edge(clk) then
                case MODE is
                    when "00" =>
                        -- Hold Value
                        reg := reg;
                    when "01" =>
                        -- Shift Right
                        reg := SI & reg(LENGTH - 1 downto 1);
                    when "10" =>
                        -- Shift Left
                        reg := reg(LENGTH - 2 downto 0) & SI;
                    when "11" =>
                        -- Parallel Load
                        reg := DIN;
                    when others =>
                        null;
                end case;
            end if;
            DOUT <= reg;
        end process main;
    
    end architecture RTL;




## ROM
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    
    entity ROM is
        port(
            address : in  std_logic_vector(3 downto 0);
            dout    : out std_logic_vector(3 downto 0)
        );
    end entity ROM;
    
    architecture RTL of ROM is
        type MEMORY_16_4 is array (0 to 15) of std_logic_vector(3 downto 0);
        constant ROM_16_4 : MEMORY_16_4 := (
            x"0",
            x"1",
            x"2",
            x"3",
            x"4",
            x"5",
            x"6",
            x"7",
            x"8",
            x"9",
            x"a",
            x"b",
            x"c",
            x"d",
            x"e",
            x"f"
        );
    begin
        main : process(address)
        begin
            dout <= ROM_16_4(to_integer(unsigned(address)));
        end process main;
    
    end architecture RTL;



## LIFO
Last In First Out (Stack) Memory

    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    
    entity LIFO is
        generic(
            WIDTH : natural := 8;
            DEPTH : natural := 128
        );
        port(
            I_DATA  : in  std_logic_vector(WIDTH - 1 downto 0); --Input Data Line
            O_DATA  : out std_logic_vector(WIDTH - 1 downto 0); --Output Data Line
            I_RD_WR : in  std_logic; --Input RD/~WR signal. 1 for READ, 0 for Write
            O_FULL  : out std_logic; --Output Full signal. 1 when memory is full.
            O_EMPTY : out std_logic; --Output Empty signal. 1 when memory is empty.
            clk     : in  std_logic;
            rst     : in  std_logic
        );
    end entity LIFO;
    
    architecture RTL of LIFO is
        -- Helper Function to convert Boolean to Std_logic
        function to_std_logic(B : boolean) return std_logic is
        begin
            if B = false then
                return '0';
            else
                return '1';
            end if;
        end function to_std_logic;
    
        type memory_type is array (0 to DEPTH - 1) of std_logic_vector(WIDTH - 1 downto 0);
        signal memory : memory_type;
    begin
        main : process(clk, rst) is
            variable stack_pointer : integer range 0 to DEPTH := 0;
            variable EMPTY, FULL   : boolean                  := false;
        begin
            --Async Reset
            if rst = '1' then
                memory   <= (others => (others => '0'));
                EMPTY := true;
                FULL  := false;
    
                stack_pointer := 0;
            elsif rising_edge(clk) then
                if I_RD_WR = '1' then
                    -- READ
                    if not EMPTY then
                        O_DATA        <= memory(stack_pointer);
                        stack_pointer := stack_pointer - 1;
                    end if;
                else
                    if stack_pointer < 16 then
                        stack_pointer          := stack_pointer + 1;
                        memory(stack_pointer - 1) <= I_DATA;
                    end if;
                end if;
    
                -- Check for Empty
                if stack_pointer = 0 then
                    EMPTY := true;
                else
                    EMPTY := false;
                end if;
    
                -- Check for Full
                if stack_pointer = DEPTH then
                    FULL := true;
                else
                    FULL := false;
                end if;
            end if;
            O_FULL  <= to_std_logic(FULL);
            O_EMPTY <= to_std_logic(EMPTY);
        end process main;
    
    end architecture RTL;



