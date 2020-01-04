----------------------------------------------------------------------------------
-- Code based on NIST LWC Schwaemm256128
-- 12/22/2019
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.utility_functions.ALL;
use work.NIST_LWAPI_pkg.ALL;

entity controller is
    Port (
        clk : in std_logic;
        rst : in std_logic;
    
        key : in std_logic_vector(31 downto 0);
        key_valid : in std_logic;
        key_ready : out std_logic;
        
        bdi : in std_logic_vector(31 downto 0);
        bdi_valid : in std_logic;
        bdi_ready : out std_logic;
        bdi_pad_loc : in std_logic_vector(3 downto 0);
        bdi_valid_bytes : in std_logic_vector(3 downto 0);
        
        bdi_size : in std_logic_vector(2 downto 0);
        bdi_eot : in std_logic;
        bdi_eoi : in std_logic;
        bdi_type : in std_logic_vector(3 downto 0);
        decrypt : in std_logic;
        hash : in std_logic;
        key_update : in std_logic;
        
        bdo : out std_logic_vector(31 downto 0);
        bdo_valid : out std_logic;
        bdo_ready : in std_logic;
        end_of_block : out std_logic;
        bdo_valid_bytes : out std_logic_vector(3 downto 0);
        bdo_type : out std_logic_vector(3 downto 0);
        
        msg_auth : out std_logic;
        msg_auth_valid : out std_logic;
        msg_auth_ready : in std_logic
    );
end controller;

architecture behavioral of controller is

    -- Controller states
    type controller_state is (IDLE, LOAD_KEY, LOAD_NPUB, INIT_STATE, 
                              LOAD_AD, LOAD_AD_ZERO, RUN_PERM, 
                              LOAD_DAT, LOAD_DAT_ZERO, FINALIZE_DAT_OUT, OUTPUT_DAT_BLK, 
                              LOAD_TAG, OUTPUT_TAG, VERIFY_TAG);
    signal current_state : controller_state;
    signal next_state : controller_state;
    
    -- Input/Output word counter
    signal word_cntr_en, word_cntr_init : std_logic;
    signal word_counter : integer;
    
    -- Signals to handle manipulation and storage of input words
    signal bdi_reg_en, key_reg_en : std_logic;
    signal bdi_pad_en, zero_fill, zero_fill_en : std_logic;

    -- Partial registers for storage of each word
    signal key_0, key_1, key_2, key_3: std_logic_vector(31 downto 0);
    signal bdi_p, bdi_z, bdi_l, bdi_c : std_logic_vector(31 downto 0);
    signal bdi_0, bdi_1, bdi_2, bdi_3, bdi_4, bdi_5, bdi_6, bdi_7 : std_logic_vector(31 downto 0);
    
    -- Signals to handle storing data once it is fully loaded
    signal key_done, key_done_en : std_logic;
    signal bdi_done, bdi_done_en : std_logic;
    
    -- Data storage registers
    signal key_reg, key_reg_prev : std_logic_vector(127 downto 0);
    signal bdi_blk_reg, bdi_blk_reg_prev : std_logic_vector(255 downto 0); 
    
    -- Signals to handle storage
    signal store_eoi, eoi_reg : std_logic;
    signal store_dec, dec_reg : std_logic;
    signal store_lblk, lblk_reg : std_logic;
    signal store_bdi_valid : std_logic;
    signal bdi_valid_reg : std_logic_vector(3 downto 0);
    signal store_lword_index : std_logic;
    signal lword_index : integer;
    
    -- BDO output signals
    signal bdo_out_reg : std_logic_vector(255 downto 0);
    signal bdo_current, bdo_current_le : std_logic_vector(31 downto 0);
    signal bdo_out_sel : std_logic;
    signal bdo_en : std_logic;
    signal bdo_valid_bytes_buf, bdo_valid_bytes_prev : std_logic_vector(3 downto 0);
    
    -- DATAPATH: Rate whitening input
    signal feistel_out : std_logic_vector(383 downto 0);                    -- Feistel unit
    signal rho_out, inv_rho_out : std_logic_vector(383 downto 0);           -- Rho, Inv rho, and Ct Rho
    signal rho_rate_in : std_logic_vector(255 downto 0); 
    signal padded_zero_pt : std_logic_vector(255 downto 0); 
    signal rho_rate_in_sel, rho_rate_in_sel_prev : std_logic;
    signal inj_const_in, inj_const_out : std_logic_vector(383 downto 0);    -- Inject constant unit
    signal inj_const_in_sel, inj_const_in_sel_prev : std_logic;
    signal pad_const : std_logic_vector(31 downto 0);                       -- Pad constant
    signal pad_const_sel, pad_const_sel_prev : std_logic_vector(1 downto 0);
    signal rate_whiten_in : std_logic_vector(383 downto 0);
    
    -- DATAPATH: AEAD data out
    signal rho_ct_out : std_logic_vector(383 downto 0);
    
    -- DATAPATH: Tag
    signal tag : std_logic_vector(127 downto 0);
    
    -- Sparkle Permutation control signals: 
    signal start_perm, start_perm_en, perm_complete : std_logic;
    signal num_steps : integer;
    signal state_sparkle_in, state_sparkle_out : std_logic_vector(383 downto 0);
    signal state_init_input, rate_whiten_out : std_logic_vector(383 downto 0);
    signal sparkle_in_sel : std_logic;
    signal state_out_hold, state_out_hold_prev : std_logic_vector(383 downto 0);
    
begin

    fesitel_unit: entity work.feistel_swap(behavioral) 
    port map(
        state_in => state_out_hold,
        state_out => feistel_out
    ); 
    
    rho_state_unit: entity work.rho(behavioral)
    port map(
        state_in => feistel_out,      
        input_rate => rho_rate_in,
        state_out => rho_out
    ); 
    
    inv_rho_unit: entity work.inv_rho(behavioral)
    port map(
        state_in_pre_feistel => state_out_hold(383 downto 128),      
        state_in_post_feistel => feistel_out,      
        input_rate => bdi_blk_reg,
        state_out => inv_rho_out
    );
    
    inject_const: entity work.inject_constant(behavioral)
    port map(
        state_in => inj_const_in,
        constant_value => pad_const,
        state_out => inj_const_out
    );
    
    rate_white_unit: entity work.rate_whitening(behavioral)
    port map(
        state_in => rate_whiten_in,
        state_out => rate_whiten_out
    ); 
    
    perm_fsm: entity work.sparkle_permutation_fsm(behavioral)
    port map (
        clk => clk,
        rst => rst,
        perm_start => start_perm,
        num_steps => num_steps,
        state_in => state_sparkle_in,
        state_out => state_sparkle_out,
        perm_complete => perm_complete
        );
    
    rho_ct_unit: entity work.rho(behavioral)
    port map(
        state_in => state_out_hold,      
        input_rate => bdi_blk_reg,
        state_out => rho_ct_out
    );
      
-- Handle BDI
bdi_z <= ZERO_W when (zero_fill = '1') else bdi;                                -- Zero fill bdi word if needed
bdi_p <= padWordLoc(bdi_z, bdi_pad_loc) when (bdi_pad_en = '1') else bdi_z;     -- Pad bdi word if needed
bdi_c <= bdi_p when (bdi_type = HDR_TAG) else littleEndianWord(bdi_p);          -- Apply little endian to bdi, except to tag

-- Assign input key, nonce, tag, ad, and dat registers
key_reg <= (key_0 & key_1 & key_2 & key_3) when (key_done = '1') else key_reg_prev;
bdi_blk_reg <= (bdi_0 & bdi_1 & bdi_2 & bdi_3 & bdi_4 & bdi_5 & bdi_6 & bdi_7) when (bdi_done = '1') else bdi_blk_reg_prev;

bdo_valid_bytes <= bdo_valid_bytes_buf;

-- Handle state initialization for AEAD
state_init_input <= bdi_blk_reg & littleEndianWord(key_reg(127 downto 96)) 
                                & littleEndianWord(key_reg(95 downto 64))
                                & littleEndianWord(key_reg(63 downto 32)) 
                                & littleEndianWord(key_reg(31 downto 0));
                             
-- MAKE THIS BETTER
padded_zero_pt <= zeroFillPt(rho_ct_out(383 downto 128), lword_index, bdo_valid_bytes_buf);

-- Assign computed tag value    
tag(127 downto 96) <= littleEndianWord(state_out_hold(127 downto 96)) xor key_reg(127 downto 96);
tag(95 downto 64) <= littleEndianWord(state_out_hold(95 downto 64)) xor key_reg(95 downto 64);
tag(63 downto 32) <= littleEndianWord(state_out_hold(63 downto 32)) xor key_reg(63 downto 32);
tag(31 downto 0) <= littleEndianWord(state_out_hold(31 downto 0)) xor key_reg(31 downto 0);

state_out_hold <= state_sparkle_out when (perm_complete = '1') else state_out_hold_prev;

-- MUX for sparkle input
with sparkle_in_sel select
state_sparkle_in <= state_init_input  when '0', 
                    rate_whiten_out when '1',
                    EMPTY_STATE when others;
                    
-- MUX for rho state input
with rho_rate_in_sel select
rho_rate_in <= bdi_blk_reg when '0',
               padded_zero_pt when '1',
               EMPTY_STATE_R when others;

-- MUX for inject constant input       
with inj_const_in_sel select
inj_const_in <= rho_out when '0',
                inv_rho_out when '1',
                EMPTY_STATE when others;

-- MUX for Sparkle number of steps
with lblk_reg select
num_steps <= STEPS_BIG when '1',
             STEPS_SMALL when '0',
             0 when others;

-- MUX for rate whiten input selection
with lblk_reg select
rate_whiten_in <= inj_const_in when '0', 
                  inj_const_out when '1', 
                  EMPTY_STATE when others;

-- MUX for pad constant select
with pad_const_sel select
pad_const <= PAD_AD_CONST when b"00", 
             NO_PAD_AD_CONST when b"01", 
             PAD_PT_CONST when b"10", 
             NO_PAD_PT_CONST when b"11", 
             ZERO_W when others;

-- MUX for bdo output (dat or tag)
with bdo_out_sel select
bdo_out_reg <= rho_ct_out(383 downto 128) when '0', 
               (tag & ZERO_W & ZERO_W & ZERO_W & ZERO_W) when '1',
                EMPTY_BDO when others;

-- MUX for bdo output (which word)
with word_counter select
bdo_current <= bdo_out_reg(255 downto 224) when 0, 
       bdo_out_reg(223 downto 192) when 1,
       bdo_out_reg(191 downto 160) when 2,
       bdo_out_reg(159 downto 128) when 3,
       bdo_out_reg(127 downto 96) when 4,
       bdo_out_reg(95 downto 64) when 5,
       bdo_out_reg(63 downto 32) when 6,
       bdo_out_reg(31 downto 0) when 7,
       ZERO_W when others;

-- MUX for bdo output (little endian vs. no little endian)
with next_state select
bdo_current_le <= bdo_current when OUTPUT_TAG,
                  littleEndianWord(bdo_current) when others;

register_input: process(clk)
begin
	if (rising_edge(clk)) then
        if (key_reg_en = '1') then
            key_3 <= key;
            key_2 <= key_3;
            key_1 <= key_2;
            key_0 <= key_1;
        end if;
        if (bdi_reg_en = '1') then
            bdi_7 <= bdi_c;
            bdi_6 <= bdi_7;
            bdi_5 <= bdi_6;
            bdi_4 <= bdi_5;
            bdi_3 <= bdi_4;
            bdi_2 <= bdi_3;
            bdi_1 <= bdi_2;
            bdi_0 <= bdi_1;
        end if;
        if (bdo_en = '1') then
            bdo <= bdo_current_le;
        else
            bdo <= x"00000000";
        end if;
    end if;
end process;

clocked_enables: process(clk)
begin
	if (rising_edge(clk)) then

	    -- Defaults
	    zero_fill <= '0';
	    start_perm <= '0';
	    key_done <= '0';
	    bdi_done <= '0';
	    	      
        if (zero_fill_en = '1') then 
            zero_fill <= '1';
        end if;
        if (start_perm_en = '1') then 
            start_perm <= '1';
        end if;
        if (bdi_done_en = '1') then 
            bdi_done <= '1';
        end if;
        if (key_done_en = '1') then 
            key_done <= '1';
        end if;
	end if;
end process;        
        
store_signals: process(clk)
begin
	if (rising_edge(clk)) then
        if (store_eoi = '1') then
            eoi_reg <= bdi_eoi;
        end if;
        if (store_lblk = '1') then 
            if (bdi_eot = '1') and (bdi_valid = '1') then
                lblk_reg <= '1';
            else
                lblk_reg <= '0';
            end if;
        end if;
        if (store_dec = '1') then
            dec_reg <= decrypt;
        end if;
        if (store_bdi_valid = '1') then
            bdi_valid_reg <= bdi_valid_bytes;
        end if;
        if (store_lword_index = '1') then
		  lword_index <= word_counter;
		end if;
	end if;
end process;

reg_process: process(clk)
begin
    if (rising_edge(clk)) then
        rho_rate_in_sel_prev <= rho_rate_in_sel;
        inj_const_in_sel_prev <= inj_const_in_sel;
        pad_const_sel_prev <= pad_const_sel;
        bdo_valid_bytes_prev <= bdo_valid_bytes_buf;
        key_reg_prev <= key_reg;
        bdi_blk_reg_prev <= bdi_blk_reg;
        state_out_hold_prev <= state_out_hold;
    end if;
end process;

counter_process: process(clk)
begin
	if (rising_edge(clk)) then
		if (word_cntr_en = '1') then
			if (word_cntr_init = '1') then
				word_counter <= 0;
			else
				word_counter <= word_counter + 1;
		    end if;
        else
            word_counter <= 0;
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

fsm_process: process(current_state, key_update, key_valid, bdi_valid, word_counter, perm_complete, bdi_type, bdi_eoi, bdi_eot, hash)
begin
 
    -- DEFAULTS:
    next_state <= current_state;                        -- FSM states
    start_perm_en <= '0';                               -- Sparkle permutation control
    word_cntr_init <= '0';                              -- Word counter
    word_cntr_en <= '0';
    bdi_pad_en <= '0';                                  -- BDI/SDI signals
    zero_fill_en <= '0';
    bdi_reg_en <= '0';
    key_reg_en <= '0';
    key_ready <= '0';                                   -- Output to preprocessor
    bdi_ready <= '0';
    bdo_en <= '0';                                      -- Output to postprocessor
    bdo_valid <= '0';
    bdo_out_sel <= '0';
    bdo_valid_bytes_buf <= bdo_valid_bytes_prev;
    msg_auth_valid <= '0';
    msg_auth <= '0';
    end_of_block <= '0';
    store_eoi <= '0';                                   -- Signals to enable storage
    store_lblk <= '0';
    store_dec <= '0';
    store_bdi_valid <= '0';
    store_lword_index <= '0';
    key_done_en <= '0';                                 -- Enable signals for loading
    bdi_done_en <= '0';
    rho_rate_in_sel <= rho_rate_in_sel_prev;            -- MUX select signals
    inj_const_in_sel <= inj_const_in_sel_prev;
    pad_const_sel <= pad_const_sel_prev;
    sparkle_in_sel <= '0';
        
    case current_state is
                     
        when IDLE => 
            if (key_update = '1') then
                if (key_valid = '1') then
                next_state <= LOAD_KEY;
                end if;
            elsif (bdi_valid = '1') then
                if (bdi_type = HDR_NPUB) then
                    next_state <= LOAD_NPUB;
                end if;
            end if;          
        
        when LOAD_KEY => 
            key_ready <= '1';                           -- Set output key ready signal
            key_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading key
            if (word_counter = KEY_WORDS - 1) then
                word_cntr_init <= '1';                  -- Reset counter value to 0
                key_done_en <= '1';                     -- Enable storage of loaded key
                
                -- Wait for NPUB data to be ready
                if (bdi_valid = '1') then
                    if (bdi_type = HDR_NPUB) then
                        next_state <= LOAD_NPUB;
                    end if;
                else 
                    next_state <= IDLE;
                end if;
            end if;
        
        when LOAD_NPUB => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading npub
            store_eoi <= '1';                           -- Enable storage of eoi
            store_dec <= '1';                           -- Enable storage of decrypt
            store_lblk <= '1';                          -- Enable storage of last block
            if (word_counter = NPUB_WORDS - 1) then
                word_cntr_init <= '1';                  -- Reset counter value to 0
                bdi_done_en <= '1';                     -- Enable storage of loaded npub
                next_state <= INIT_STATE;
                start_perm_en <= '1';
            end if;
            
        when INIT_STATE => 
            if (perm_complete = '1') then               -- Wait for completion
                if (eoi_reg = '1') then                 -- If end of input, handle tag based on enc or dec
                    if (dec_reg = '1') then
                        next_state <= LOAD_TAG;         -- If dec, then load tag from input
                    else
                        next_state <= OUTPUT_TAG;       -- If enc, then transition to outputting calculated tag
                        bdo_out_sel <= '1';             -- Select tag for BDO output
                        word_cntr_en <= '1';            -- Enable word counter
                        bdo_en <= '1';                  -- Enable bdo output
                    end if;
                else                                    -- If NOT end of input, transition to loading AD or DAT
                    if (bdi_valid = '1') and (bdi_type = HDR_AD) then
                        next_state <= LOAD_AD;
                    elsif (bdi_valid = '1') and ((bdi_type = HDR_MSG) or (bdi_type = HDR_CT)) then
                        next_state <= LOAD_DAT;
                    end if;
                end if;
            end if;
        
        when LOAD_AD => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading AD
            store_eoi <= '1';                           -- Enable storage of eoi
            store_lblk <= '1';                          -- Enable storage of last block
            rho_rate_in_sel <= '0';                     -- Rho input should be current bdi block
            inj_const_in_sel <= '0';                    -- Inject const input should be rho output
            
            -- Handle padding of current word
            if (bdi_valid_bytes /= VALID_WORD) then
                bdi_pad_en <= '1';                      -- If the current block is not all valid, enable padding
                pad_const_sel <= b"00";                 -- Update pad constant select: PAD
            else
                pad_const_sel <= b"01";                 -- Update pad constant select: NO PAD
            end if;
            
            -- Handle end of input block
            if (word_counter = AD_BLK_WORDS - 1) then   -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                bdi_done_en <= '1';                     -- Enable storage of loaded AD
                next_state <= RUN_PERM;
                start_perm_en <= '1';                   
            else                                        -- Block still loading
                if (bdi_eot = '1') then                 -- Handle incomplete last input block
                    zero_fill_en <= '1';                -- Enable zero fill for the rest of block
                    next_state <= LOAD_AD_ZERO;         -- Zero fill the rest of the block
                end if; 
            end if;
            
        when LOAD_AD_ZERO => 
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading ad
            zero_fill_en <= '1';                        -- Enable zero fill for the rest of block
            
            -- If previous word was all valid, pad the zero-filled word
            if (bdi_valid_bytes = VALID_WORD) then
                bdi_pad_en <= '1';
                pad_const_sel <= b"00";
            end if;
                
            if (word_counter = AD_BLK_WORDS - 1) then   -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                bdi_done_en <= '1';                     -- Enable storage of loaded AD
                next_state <= RUN_PERM;  
                start_perm_en <= '1';
            end if;
            
        when RUN_PERM =>             
            sparkle_in_sel <= '1';                      -- Select rate whitening output as state input
            if (perm_complete = '1') then               -- Wait for completion
                if (eoi_reg = '1') then                 -- If end of input, handle tag based on enc or dec
                    if (dec_reg = '1') then
                        next_state <= LOAD_TAG;         -- If dec, load tag from input
                    else
                        bdo_out_sel <= '1';             -- Set BDO output to tag
                        bdo_en <= '1';                  -- Enable output
                        word_cntr_en <= '1';            -- Enable word counter
                        next_state <= OUTPUT_TAG;
                    end if;
                else                                    -- If NOT end of input, transition to loading AD or DAT
                    if (bdi_valid = '1') and (bdi_type = HDR_AD) then
                        next_state <= LOAD_AD;
                    elsif (bdi_valid = '1') and ((bdi_type = HDR_MSG) or (bdi_type = HDR_CT)) then
                        next_state <= LOAD_DAT;
                    end if;
                end if;
            end if;
            
        when LOAD_DAT => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading ad
            store_eoi <= '1';                           -- Enable storage of eoi
            store_lblk <= '1';                          -- Enable storage of last block
            store_bdi_valid <= '1';                     -- Enable storage of bdi valid bytes
            store_lword_index <= '1';                   -- Enable storage of index of the last word
            
            inj_const_in_sel <= '0';
            
            -- Handle padding of current word
            if (bdi_valid_bytes /= VALID_WORD) then
                bdi_pad_en <= '1';                      -- If the current block is not all valid, enable padding
                pad_const_sel <= b"10";                 -- Update pad constant select: PAD
            else
                pad_const_sel <= b"11";                 -- Update pad constant select: NO PAD
            end if;
            
            -- Handle end of input block
            if (word_counter = DAT_BLK_WORDS - 1) then  -- Full block loaded

                -- If decrypting completely valid full block use inv rho, else use rho with padded PT input 
                if (dec_reg = '1') and (bdi_valid_bytes = VALID_WORD) then
                    inj_const_in_sel <= '1';
                elsif (dec_reg = '1') and (bdi_valid_bytes /= VALID_WORD) then
                    rho_rate_in_sel <= '1';
                end if;      

                word_cntr_init <= '1';                  -- Reset counter value to 0
                bdi_done_en <= '1';                     -- Enable storage of loaded DAT
                next_state <= FINALIZE_DAT_OUT;
            else                                        -- Block still loading
                if (bdi_eot = '1') then                 -- Handle incomplete last input block
                    if (dec_reg = '1') then             -- Decryption and padding required, select padded pt rho rate input
                        rho_rate_in_sel <= '1';
                    end if;
                    zero_fill_en <= '1';                -- Enable zero fill for the rest of block
                    next_state <= LOAD_DAT_ZERO;        -- Zero fill the rest of the block
                end if; 
            end if;
        
        when LOAD_DAT_ZERO => 
            bdi_reg_en <= '1';                          -- Enable storage of each word
            word_cntr_en <= '1';                        -- Keep word counter enabled while loading ad
            zero_fill_en <= '1';                        -- Enable zero fill for the rest of block
            
            -- If previous word was all valid, pad the zero-filled word
            if (bdi_valid_bytes = VALID_WORD) then
                bdi_pad_en <= '1';
                pad_const_sel <= b"10";
            end if;

            if (word_counter = DAT_BLK_WORDS - 1) then  -- Full block loaded
                word_cntr_init <= '1';                  -- Reset counter value to 0
                bdi_done_en <= '1';                     -- Enable storage of loaded DAT
                next_state <= FINALIZE_DAT_OUT;
            end if;
            
        when FINALIZE_DAT_OUT => 
            bdo_en <= '1';                              -- Enable output
            word_cntr_en <= '1';                        -- Enable word counter
            next_state <= OUTPUT_DAT_BLK;
            
        when OUTPUT_DAT_BLK => 
            bdo_en <= '1';                              -- Enable output
            bdo_valid <= '1';                           -- Set bdo valid flag
            bdo_valid_bytes_buf <= VALID_WORD;          -- Set output word to valid
            word_cntr_en <= '1';                        -- Keep word counter enabled while outputting data
                        
            if (word_counter = lword_index + 1) then    -- End of block
                end_of_block <= '1';                    -- Indicate end of output block
                bdo_valid_bytes_buf <= bdi_valid_reg;   -- Set bdo valid bytes for last word
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= RUN_PERM;
                start_perm_en <= '1';
            end if;
            
        when LOAD_TAG => 
            bdi_ready <= '1';                           -- Set output bdi ready signal
            if (bdi_type = HDR_TAG) then
                bdi_reg_en <= '1';                      -- Enable storage of each word
                word_cntr_en <= '1';                    -- Keep word counter enabled while loading tag
                if (word_counter = TAG_WORDS - 1) then
                    word_cntr_init <= '1';              -- Reset counter value to 0
                    bdi_done_en <= '1';                 -- Enable storage of loaded tag
                    next_state <= VERIFY_TAG;
                end if;
            end if;
            
        when OUTPUT_TAG =>            
            bdo_out_sel <= '1';                         -- Hold BDO output at data
            bdo_en <= '1';                              -- Enable output
            bdo_valid <= '1';                           -- Set bdo valid flag
            bdo_valid_bytes_buf <= VALID_WORD;          -- Set output word to valid
            word_cntr_en <= '1';                        -- Keep word counter enabled while outputting data
                        
            if (word_counter = TAG_WORDS) then          
                end_of_block <= '1';                    -- Indicate end of output tag block
                word_cntr_init <= '1';                  -- Reset counter value to 0
                next_state <= IDLE;
            end if;
        
        when VERIFY_TAG => 
            msg_auth_valid <= '1';                      -- Indicate msg auth output is valid
            if (tag = bdi_blk_reg(127 downto 0)) then   -- Output result of tag comparison
                msg_auth <= '1';
            else
                msg_auth <= '0';
            end if;
            next_state <= IDLE;
            
        when others =>
            next_state <= IDLE;
    end case; 

end process;
end behavioral;
