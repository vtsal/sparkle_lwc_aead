----------------------------------------------------------------------------------
-- Code based on NIST LWC Schwaemm256128
-- 12/22/2019
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.utility_functions.ALL;

entity arx_round is
Port ( 
    round_constant : in std_logic_vector(31 downto 0);
    x_round_in, y_round_in : in std_logic_vector(31 downto 0);
    x_rot, y_rot : in integer;
    x_round_out, y_round_out : out std_logic_vector(31 downto 0)
    );
end arx_round;

architecture behavioral of arx_round is
begin

    -- Perform Round: 
    y_round_out <= y_round_in xor rotWordRight((x_round_in + rotWordRight(y_round_in, y_rot)), x_rot);
    x_round_out <= (x_round_in + rotWordRight(y_round_in, y_rot)) xor round_constant;
    
end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use work.utility_functions.ALL;

entity linear_layer is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end linear_layer;

architecture behavioral of linear_layer is

    -- Function to compute x tmp or y tmp
    function ell ( in_0, in_1, in_2 : in std_logic_vector(31 downto 0))
    return std_logic_vector is variable tmp : std_logic_vector(31 downto 0);
    begin 
        tmp := rotWordRight((((in_0(15 downto 0) xor in_1(15 downto 0) xor in_2(15 downto 0)) & x"0000") xor (in_0 xor in_1 xor in_2)), 16);
        return tmp;
    end ell;

begin

    state_out(351 downto 320) <= state_in(287 downto 256) xor state_in(95 downto 64) xor ell(state_in(383 downto 352), state_in(319 downto 288), state_in(255 downto 224));
    state_out(287 downto 256) <= state_in(223 downto 192) xor state_in(31 downto 0) xor ell(state_in(383 downto 352), state_in(319 downto 288), state_in(255 downto 224));
    state_out(223 downto 192) <= state_in(351 downto 320) xor state_in(159 downto 128) xor ell(state_in(383 downto 352), state_in(319 downto 288), state_in(255 downto 224));
    state_out(159 downto 128) <= state_in(351 downto 320);
    state_out(95 downto 64) <= state_in(287 downto 256);
    state_out(31 downto 0) <= state_in(223 downto 192);
    
    state_out(383 downto 352) <= state_in(319 downto 288) xor state_in(127 downto 96) xor ell(state_in(351 downto 320), state_in(287 downto 256), state_in(223 downto 192));
    state_out(319 downto 288) <= state_in(255 downto 224) xor state_in(63 downto 32) xor ell(state_in(351 downto 320), state_in(287 downto 256), state_in(223 downto 192));
    state_out(255 downto 224) <= state_in(383 downto 352) xor state_in(191 downto 160) xor ell(state_in(351 downto 320), state_in(287 downto 256), state_in(223 downto 192));
    state_out(191 downto 160) <= state_in(383 downto 352);
    state_out(127 downto 96) <= state_in(319 downto 288);
    state_out(63 downto 32) <= state_in(255 downto 224);

end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utility_functions.ALL;

entity sparkle_permutation_fsm is
    Port ( 
        clk : in std_logic;
        rst : in std_logic;
        perm_start : in std_logic;
        num_steps : in integer;
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0);
        perm_complete : out std_logic
        );
end sparkle_permutation_fsm;

architecture behavioral of sparkle_permutation_fsm is

    type word_constants is array(0 to 7) of std_logic_vector (31 downto 0);
       constant round_constant_array : word_constants := (x"B7E15162", x"BF715880", x"38B4DA56", x"324E7738",
                                                          x"BB1185EB", x"4F7C7B57", x"CFBFA1C8", x"C2B3293D");
                                                        
    type integer_constants is array(0 to 4) of integer;
        constant y_rot_array : integer_constants := (31, 17, 0, 24, 0);
        constant x_rot_array : integer_constants := (24, 17, 31, 16, 0);
	
	type perm_state is (IDLE, RUN);
        signal current_state : perm_state;
        signal next_state : perm_state;
        
	signal arx_state_in, arx_state_out : std_logic_vector(383 downto 0);
	signal x_rot_signal, y_rot_signal : integer := 0;
	signal round_counter : integer := 0;
	
	signal linear_state_in, linear_state_out : std_logic_vector(383 downto 0);
	signal step_counter : integer := 0;
	
	signal state_storage_reg : std_logic_vector(383 downto 0);
	
	signal arx_cntr_en, arx_cntr_init, perm_cntr_en, perm_cntr_init : std_logic;
	signal outval_from_reg : std_logic;
	signal lin_en, reg_en : std_logic;
	
begin

    -- Map x input signals
    arx_state_in(383 downto 352) <= state_in(383 downto 352) when (perm_start = '1') else state_storage_reg(383 downto 352);
    arx_state_in(319 downto 288) <= state_in(319 downto 288) when (perm_start = '1') else state_storage_reg(319 downto 288);
    arx_state_in(255 downto 224) <= state_in(255 downto 224) when (perm_start = '1') else state_storage_reg(255 downto 224);
    arx_state_in(191 downto 160) <= state_in(191 downto 160) when (perm_start = '1') else state_storage_reg(191 downto 160);
    arx_state_in(127 downto 96) <= state_in(127 downto 96) when (perm_start = '1') else state_storage_reg(127 downto 96);
    arx_state_in(63 downto 32) <= state_in(63 downto 32) when (perm_start = '1') else state_storage_reg(63 downto 32);
    
    -- Map y input signals
    arx_state_in(351 downto 320) <= (state_in(351 downto 320) xor round_constant_array(step_counter mod 8)) when (perm_start = '1') else state_storage_reg(351 downto 320);
    arx_state_in(287 downto 256) <= (state_in(287 downto 256) xor std_logic_vector(to_unsigned(step_counter, 32))) when (perm_start = '1') else state_storage_reg(287 downto 256);
    arx_state_in(223 downto 192) <= state_in(223 downto 192) when (perm_start = '1') else state_storage_reg(223 downto 192);
    arx_state_in(159 downto 128) <= state_in(159 downto 128) when (perm_start = '1') else state_storage_reg(159 downto 128);
    arx_state_in(95 downto 64) <= state_in(95 downto 64) when (perm_start = '1') else state_storage_reg(95 downto 64);
    arx_state_in(31 downto 0) <= state_in(31 downto 0) when (perm_start = '1') else state_storage_reg(31 downto 0);
    
    -- Map rotation amounts for arx rounds
    x_rot_signal <= x_rot_array(round_counter);
    y_rot_signal <= y_rot_array(round_counter);

    -- Map linear layer input
    linear_state_in <= state_storage_reg;
    
    -- Map output state
    state_out <= state_storage_reg when (outval_from_reg = '1') else linear_state_out;
--    state_out <= state_in when (outval_from_reg = '1') else linear_state_out;
    
    arx_round_unit_0: entity work.arx_round(behavioral)
	port map(
	    round_constant => round_constant_array(0),
        x_round_in => arx_state_in(383 downto 352),
        y_round_in => arx_state_in(351 downto 320),
        x_rot => x_rot_signal,
        y_rot => y_rot_signal,
        x_round_out => arx_state_out(383 downto 352),
        y_round_out => arx_state_out(351 downto 320)
		);
		
    arx_round_unit_1: entity work.arx_round(behavioral)
	port map(
	    round_constant => round_constant_array(1),
        x_round_in => arx_state_in(319 downto 288),
        y_round_in => arx_state_in(287 downto 256),
        x_rot => x_rot_signal,
        y_rot => y_rot_signal,
        x_round_out => arx_state_out(319 downto 288),
        y_round_out => arx_state_out(287 downto 256)
		);
		
    arx_round_unit_2: entity work.arx_round(behavioral)
	port map(
	    round_constant => round_constant_array(2),
        x_round_in => arx_state_in(255 downto 224),
        y_round_in => arx_state_in(223 downto 192),
        x_rot => x_rot_signal,
        y_rot => y_rot_signal,
        x_round_out => arx_state_out(255 downto 224),
        y_round_out => arx_state_out(223 downto 192)
		);
		
    arx_round_unit_3: entity work.arx_round(behavioral)
	port map(
	    round_constant => round_constant_array(3),
        x_round_in => arx_state_in(191 downto 160),
        y_round_in => arx_state_in(159 downto 128),
        x_rot => x_rot_signal,
        y_rot => y_rot_signal,
        x_round_out => arx_state_out(191 downto 160),
        y_round_out => arx_state_out(159 downto 128)
		);
		
    arx_round_unit_4: entity work.arx_round(behavioral)
	port map(
	    round_constant => round_constant_array(4),
        x_round_in => arx_state_in(127 downto 96),
        y_round_in => arx_state_in(95 downto 64),
        x_rot => x_rot_signal,
        y_rot => y_rot_signal,
        x_round_out => arx_state_out(127 downto 96),
        y_round_out => arx_state_out(95 downto 64)
		);
		
    arx_round_unit_5: entity work.arx_round(behavioral)
	port map(
	    round_constant => round_constant_array(5),
        x_round_in => arx_state_in(63 downto 32),
        y_round_in => arx_state_in(31 downto 0),
        x_rot => x_rot_signal,
        y_rot => y_rot_signal,
        x_round_out => arx_state_out(63 downto 32),
        y_round_out => arx_state_out(31 downto 0)
		);

    linear_layer_unit: entity work.linear_layer(behavioral)
	port map(
	    state_in => linear_state_in,
        state_out => linear_state_out
		);
			
reg_process: process(clk)
begin
	if (rising_edge(clk)) then
		if (reg_en = '1') then
			if (lin_en = '1') then

                -- Map the x branch values
				state_storage_reg(383 downto 352) <= linear_state_out(383 downto 352);
				state_storage_reg(319 downto 288) <= linear_state_out(319 downto 288);
				state_storage_reg(255 downto 224) <= linear_state_out(255 downto 224);
				state_storage_reg(191 downto 160) <= linear_state_out(191 downto 160);
				state_storage_reg(127 downto 96) <= linear_state_out(127 downto 96);
				state_storage_reg(63 downto 32) <= linear_state_out(63 downto 32);
                
                -- If you have reached the last step of the permutation, do not update y0 and y1
                if (step_counter = (num_steps - 1)) then
                    state_storage_reg(351 downto 320) <= linear_state_out(351 downto 320);
				    state_storage_reg(287 downto 256) <= linear_state_out(287 downto 256);
				-- All other steps require y0 and y1 to be updated prior to arx box
                else
				    state_storage_reg(351 downto 320) <= linear_state_out(351 downto 320) xor round_constant_array((step_counter + 1) mod 8);
				    state_storage_reg(287 downto 256) <= linear_state_out(287 downto 256) xor std_logic_vector(to_unsigned((step_counter + 1), 32));
				end if;
				
				state_storage_reg(223 downto 192) <= linear_state_out(223 downto 192);
				state_storage_reg(159 downto 128) <= linear_state_out(159 downto 128);
				state_storage_reg(95 downto 64) <= linear_state_out(95 downto 64);
				state_storage_reg(31 downto 0) <= linear_state_out(31 downto 0);
			else
				state_storage_reg <= arx_state_out;
		    end if;
		end if;
	end if;
end process;

counter_process: process(clk)
begin
	if (rising_edge(clk)) then
		if (arx_cntr_en = '1') then
			if (arx_cntr_init = '1') then
				round_counter <= 0;
			else
				round_counter <= round_counter + 1;
		    end if;
		end if;
		if (perm_cntr_en = '1') then
			if (perm_cntr_init = '1') then
				step_counter <= 0;
			else
				step_counter <= step_counter + 1;
		    end if;
		end if;
	end if;
end process;
	
sync_process: process(clk)
begin

if (rising_edge(clk)) then
	if (rst = '1') then
	   current_state <= IDLE;
	else
	   current_state <= next_state;
	end if;
end if;

end process;

public_process: process(current_state, perm_start, round_counter, step_counter)
begin
 
-- defaults
lin_en <= '0'; 
arx_cntr_init <= '0';
arx_cntr_en <= '0';
perm_cntr_init <= '0';
perm_cntr_en <= '0';
perm_complete <= '0';
reg_en <= '0';
outval_from_reg <= '1';

case current_state is
		 		 
	 when IDLE => 
		if (perm_start = '1') then
			arx_cntr_en <= '1';
			reg_en <= '1';
			next_state <= RUN;
		else
			next_state <= IDLE;
		end if;
	    
    when RUN => 
        arx_cntr_en <= '1';
        reg_en <= '1';
        
        if (round_counter = 4) then
        
            lin_en <= '1';
            arx_cntr_init <= '1';   -- Reset the arx round counter
            perm_cntr_en <= '1';    -- Enable the step counter
           
            if (step_counter = (num_steps - 1)) then
            
                perm_complete <= '1';       -- Update permutation complete flag
                perm_cntr_init <= '1';      -- Reset the permutation step counter
                outval_from_reg <= '0';
				next_state <= IDLE;
			else
                next_state <= RUN;          -- If the number of steps has not completed, continue
			end if;
		else
		    perm_cntr_en <= '0';
            next_state <= RUN;
		end if;

    when others =>
		next_state <= IDLE;
			  
end case; 

end process;
end behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity rate_whitening is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end rate_whitening;

architecture behavioral of rate_whitening is
begin

    -- Update rate portion of state
    state_out(383 downto 352) <= state_in(383 downto 352) xor state_in(127 downto 96);
    state_out(351 downto 320) <= state_in(351 downto 320) xor state_in(95 downto 64);
    state_out(319 downto 288) <= state_in(319 downto 288) xor state_in(63 downto 32);
    state_out(287 downto 256) <= state_in(287 downto 256) xor state_in(31 downto 0);
    state_out(255 downto 224) <= state_in(255 downto 224) xor state_in(127 downto 96);
    state_out(223 downto 192) <= state_in(223 downto 192) xor state_in(95 downto 64);
    state_out(191 downto 160) <= state_in(191 downto 160) xor state_in(63 downto 32);
    state_out(159 downto 128) <= state_in(159 downto 128) xor state_in(31 downto 0);
    
    -- Capacity portion of state not modified
    state_out(127 downto 96) <= state_in(127 downto 96);
    state_out(95 downto 64) <= state_in(95 downto 64);
    state_out(63 downto 32) <= state_in(63 downto 32);
    state_out(31 downto 0) <= state_in(31 downto 0);

end behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.utility_functions.ALL;

entity inject_constant is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        constant_value : in std_logic_vector(31 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end inject_constant;

architecture behavioral of inject_constant is
begin

    -- Map state in to state out
    state_out(383 downto 352) <= state_in(383 downto 352);
    state_out(351 downto 320) <= state_in(351 downto 320);
    state_out(319 downto 288) <= state_in(319 downto 288);
    state_out(287 downto 256) <= state_in(287 downto 256);
    state_out(255 downto 224) <= state_in(255 downto 224);
    state_out(223 downto 192) <= state_in(223 downto 192);
    state_out(191 downto 160) <= state_in(191 downto 160);
    state_out(159 downto 128) <= state_in(159 downto 128);
    state_out(127 downto 96) <= state_in(127 downto 96);
    state_out(95 downto 64) <= state_in(95 downto 64);
    state_out(63 downto 32) <= state_in(63 downto 32);
    
    -- Update last word of state with constant
    state_out(31 downto 0) <= state_in(31 downto 0) xor constant_value;

end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity feistel_swap is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end feistel_swap;

architecture behavioral of feistel_swap is
begin

    -- Update rate part of state
    state_out(383 downto 352) <= state_in(255 downto 224);
    state_out(351 downto 320) <= state_in(223 downto 192);
    state_out(319 downto 288) <= state_in(191 downto 160);
    state_out(287 downto 256) <= state_in(159 downto 128);
    state_out(255 downto 224) <= state_in(255 downto 224) xor state_in(383 downto 352);
    state_out(223 downto 192) <= state_in(223 downto 192) xor state_in(351 downto 320);
    state_out(191 downto 160) <= state_in(191 downto 160) xor state_in(319 downto 288);
    state_out(159 downto 128) <= state_in(159 downto 128) xor state_in(287 downto 256);
    
    -- Rightmost 4 words not affected
    state_out(127 downto 96) <= state_in(127 downto 96);
    state_out(95 downto 64) <= state_in(95 downto 64);
    state_out(63 downto 32) <= state_in(63 downto 32);
    state_out(31 downto 0) <= state_in(31 downto 0);
    
end behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity rho is
    Port (
        state_in : in std_logic_vector(383 downto 0);
        input_rate : in std_logic_vector(255 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end rho;

architecture behavioral of rho is
begin

    -- Update rate part of state
    state_out(383 downto 352) <= state_in(383 downto 352) xor input_rate(255 downto 224);
    state_out(351 downto 320) <= state_in(351 downto 320) xor input_rate(223 downto 192);
    state_out(319 downto 288) <= state_in(319 downto 288) xor input_rate(191 downto 160);
    state_out(287 downto 256) <= state_in(287 downto 256) xor input_rate(159 downto 128);
    state_out(255 downto 224) <= state_in(255 downto 224) xor input_rate(127 downto 96);
    state_out(223 downto 192) <= state_in(223 downto 192) xor input_rate(95 downto 64);
    state_out(191 downto 160) <= state_in(191 downto 160) xor input_rate(63 downto 32);
    state_out(159 downto 128) <= state_in(159 downto 128) xor input_rate(31 downto 0);
    
    -- Capacity not affected
    state_out(127 downto 96) <= state_in(127 downto 96);
    state_out(95 downto 64) <= state_in(95 downto 64);
    state_out(63 downto 32) <= state_in(63 downto 32);
    state_out(31 downto 0) <= state_in(31 downto 0);
    
end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;

entity inv_rho is
    Port (
        state_in_pre_feistel : in std_logic_vector(255 downto 0);
        state_in_post_feistel : in std_logic_vector(383 downto 0);
        input_rate : in std_logic_vector(255 downto 0);
        state_out : out std_logic_vector(383 downto 0)
    );
end inv_rho;

architecture behavioral of inv_rho is
begin

    -- Update rate part of state
    state_out(383 downto 352) <= state_in_post_feistel(383 downto 352) xor (state_in_pre_feistel(255 downto 224) xor input_rate(255 downto 224));
    state_out(351 downto 320) <= state_in_post_feistel(351 downto 320) xor (state_in_pre_feistel(223 downto 192) xor input_rate(223 downto 192));
    state_out(319 downto 288) <= state_in_post_feistel(319 downto 288) xor (state_in_pre_feistel(191 downto 160) xor input_rate(191 downto 160));
    state_out(287 downto 256) <= state_in_post_feistel(287 downto 256) xor (state_in_pre_feistel(159 downto 128) xor input_rate(159 downto 128));
    state_out(255 downto 224) <= state_in_post_feistel(255 downto 224) xor (state_in_pre_feistel(127 downto 96) xor input_rate(127 downto 96));
    state_out(223 downto 192) <= state_in_post_feistel(223 downto 192) xor (state_in_pre_feistel(95 downto 64) xor input_rate(95 downto 64));
    state_out(191 downto 160) <= state_in_post_feistel(191 downto 160) xor (state_in_pre_feistel(63 downto 32) xor input_rate(63 downto 32));
    state_out(159 downto 128) <= state_in_post_feistel(159 downto 128) xor (state_in_pre_feistel(31 downto 0) xor input_rate(31 downto 0));
    
    -- Capacity not affected
    state_out(127 downto 96) <= state_in_post_feistel(127 downto 96);
    state_out(95 downto 64) <= state_in_post_feistel(95 downto 64);
    state_out(63 downto 32) <= state_in_post_feistel(63 downto 32);
    state_out(31 downto 0) <= state_in_post_feistel(31 downto 0);
    
end behavioral;

