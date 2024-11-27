library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
Library xpm;
use xpm.vcomponents.all;

entity full_radio_v1_0_S00_AXI is
	generic (
		-- Users to add parameters here

		-- User parameters ends
		-- Do not modify the parameters beyond this line

		-- Width of S_AXI data bus
		C_S_AXI_DATA_WIDTH	: integer	:= 32;
		-- Width of S_AXI address bus
		C_S_AXI_ADDR_WIDTH	: integer	:= 4
	);
	port (
		-- Users to add ports here
        m_axis_tdata : out std_logic_vector(31 downto 0);
        m_axis_tvalid : out std_logic;
		-- User ports ends
		-- Do not modify the ports beyond this line

		-- Global Clock Signal
		S_AXI_ACLK	: in std_logic;
		-- Global Reset Signal. This Signal is Active LOW
		S_AXI_ARESETN	: in std_logic;
		-- Write address (issued by master, acceped by Slave)
		S_AXI_AWADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Write channel Protection type. This signal indicates the
    		-- privilege and security level of the transaction, and whether
    		-- the transaction is a data access or an instruction access.
		S_AXI_AWPROT	: in std_logic_vector(2 downto 0);
		-- Write address valid. This signal indicates that the master signaling
    		-- valid write address and control information.
		S_AXI_AWVALID	: in std_logic;
		-- Write address ready. This signal indicates that the slave is ready
    		-- to accept an address and associated control signals.
		S_AXI_AWREADY	: out std_logic;
		-- Write data (issued by master, acceped by Slave) 
		S_AXI_WDATA	: in std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Write strobes. This signal indicates which byte lanes hold
    		-- valid data. There is one write strobe bit for each eight
    		-- bits of the write data bus.    
		S_AXI_WSTRB	: in std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
		-- Write valid. This signal indicates that valid write
    		-- data and strobes are available.
		S_AXI_WVALID	: in std_logic;
		-- Write ready. This signal indicates that the slave
    		-- can accept the write data.
		S_AXI_WREADY	: out std_logic;
		-- Write response. This signal indicates the status
    		-- of the write transaction.
		S_AXI_BRESP	: out std_logic_vector(1 downto 0);
		-- Write response valid. This signal indicates that the channel
    		-- is signaling a valid write response.
		S_AXI_BVALID	: out std_logic;
		-- Response ready. This signal indicates that the master
    		-- can accept a write response.
		S_AXI_BREADY	: in std_logic;
		-- Read address (issued by master, acceped by Slave)
		S_AXI_ARADDR	: in std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
		-- Protection type. This signal indicates the privilege
    		-- and security level of the transaction, and whether the
    		-- transaction is a data access or an instruction access.
		S_AXI_ARPROT	: in std_logic_vector(2 downto 0);
		-- Read address valid. This signal indicates that the channel
    		-- is signaling valid read address and control information.
		S_AXI_ARVALID	: in std_logic;
		-- Read address ready. This signal indicates that the slave is
    		-- ready to accept an address and associated control signals.
		S_AXI_ARREADY	: out std_logic;
		-- Read data (issued by slave)
		S_AXI_RDATA	: out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
		-- Read response. This signal indicates the status of the
    		-- read transfer.
		S_AXI_RRESP	: out std_logic_vector(1 downto 0);
		-- Read valid. This signal indicates that the channel is
    		-- signaling the required read data.
		S_AXI_RVALID	: out std_logic;
		-- Read ready. This signal indicates that the master can
    		-- accept the read data and response information.
		S_AXI_RREADY	: in std_logic
	);
end full_radio_v1_0_S00_AXI;

architecture arch_imp of full_radio_v1_0_S00_AXI is

	-- AXI4LITE signals
	signal axi_awaddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_awready	: std_logic;
	signal axi_wready	: std_logic;
	signal axi_bresp	: std_logic_vector(1 downto 0);
	signal axi_bvalid	: std_logic;
	signal axi_araddr	: std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
	signal axi_arready	: std_logic;
	signal axi_rdata	: std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal axi_rresp	: std_logic_vector(1 downto 0);
	signal axi_rvalid	: std_logic;

	-- Example-specific design signals
	-- local parameter for addressing 32 bit / 64 bit C_S_AXI_DATA_WIDTH
	-- ADDR_LSB is used for addressing 32/64 bit registers/memories
	-- ADDR_LSB = 2 for 32 bits (n downto 2)
	-- ADDR_LSB = 3 for 64 bits (n downto 3)
	constant ADDR_LSB  : integer := (C_S_AXI_DATA_WIDTH/32)+ 1;
	constant OPT_MEM_ADDR_BITS : integer := 1;
	------------------------------------------------
	---- Signals for user logic register space example
	--------------------------------------------------
	---- Number of Slave Registers 4
	signal slv_reg0	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg1	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg2	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg3	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal slv_reg_rden	: std_logic;
	signal slv_reg_wren	: std_logic;
	signal reg_data_out	:std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
	signal byte_index	: integer;
	signal aw_en	: std_logic;
  
-- DDS Compilers start

-- Fake ADC (dds_compiler)
  component dds_compiler is
  port (
    aclk : IN STD_LOGIC;
    aresetn : IN STD_LOGIC;
    s_axis_phase_tvalid : IN STD_LOGIC;
    s_axis_phase_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axis_data_tvalid : OUT STD_LOGIC;
    m_axis_data_tdata : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
  end component dds_compiler;

-- Tuner
  component tunerDds is
  port (
    aclk : IN STD_LOGIC;
    aresetn : IN STD_LOGIC;
    s_axis_phase_tvalid : IN STD_LOGIC;
    s_axis_phase_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axis_data_tvalid : OUT STD_LOGIC;
    m_axis_data_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
  );
  end component tunerDds;
-- DDS Compilers end

-- Add FIR 1
COMPONENT fir_compiler_0
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_data_tvalid : IN STD_LOGIC;
    s_axis_data_tready : OUT STD_LOGIC;
    s_axis_data_tdata : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    m_axis_data_tvalid : OUT STD_LOGIC;
    m_axis_data_tdata : OUT STD_LOGIC_VECTOR(47 DOWNTO 0) 
  );
END COMPONENT;
-- Add FIR 2
COMPONENT fir_compiler_1
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_data_tvalid : IN STD_LOGIC;
    s_axis_data_tready : OUT STD_LOGIC;
    s_axis_data_tdata : IN STD_LOGIC_VECTOR(47 DOWNTO 0);
    m_axis_data_tvalid : OUT STD_LOGIC;
    m_axis_data_tdata : OUT STD_LOGIC_VECTOR(71 DOWNTO 0) 
  );
END COMPONENT;

----Add ALL the wiring (signals)

--DDS (Fake ADC and Tuner) -------------------------------------------------------------------- 


  -- Define Fake ADC (DDS Compiler) signals
  signal i_ddsFreq_tdata : STD_LOGIC_VECTOR(15 downto 0) := (others=>'0');
  signal wireValidPhase, resetn_ddsc_sysclk, i_ddsFreq_tvalid : STD_LOGIC;
  signal i_ddsReset : STD_LOGIC := '1';
  signal timeSinceReset : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0');
      
  -- Define Tuner (DDS Compiler) signals
  signal dds_cos : STD_LOGIC_VECTOR(15 downto 0) := (others=>'0'); --real
  signal dds_sin : STD_LOGIC_VECTOR(15 downto 0) := (others=>'0'); --imaginary
  signal wireValidPhaseTuner, resetn_ddsc_sysclkTuner, i_ddsFreq_tvalidTuner : STD_LOGIC;
  signal i_ddsResetTuner : STD_LOGIC := '1';

  -- Complex Multiplier --------------------------------------------------------------------
  -- integers
  signal int_fakeADC : integer ;
  signal int_tuner_dds_cos : integer ;
  signal int_tuner_dds_sin : integer ;
  signal int_i_real_output : integer ;
  signal int_q_imaginary_output : integer ;
  
  -- Real (cosine, I) & Imaginary (sine, Q)
  signal output_i_32 : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0');
  signal output_q_32 : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0');
  
  -- FIR Imaginary --------------------------------------------------------------------
  -- Wires for FIR 0
  signal wireToFir0_q_imaginary_tdata : STD_LOGIC_VECTOR ( 15 downto 0 );
  signal wireToFir0_q_imaginary_tvalid : STD_LOGIC;
  signal wireToFir0_q_imaginary_tready : STD_LOGIC;
  
  -- FIR 0 to 1
  signal wireFir0ToFir1_q_imaginary_tdata : STD_LOGIC_VECTOR ( 47 downto 0 );
  signal wireFir0ToFir1_q_imaginary_tvalid : STD_LOGIC;
  
  -- Wires out of FIR 1
  signal wireToFir1_q_imaginary_tready : STD_LOGIC;
  signal wireFromFir1_q_imaginary_tdata : STD_LOGIC_VECTOR (71 DOWNTO 0) := (others=>'0');
  signal wireFromFir1_q_imaginary_tvalid : STD_LOGIC;
  
  -- FIR Real --------------------------------------------------------------------
  -- Wires for FIR 0
  signal wireToFir0_i_real_tdata : STD_LOGIC_VECTOR ( 15 downto 0 );
  signal wireToFir0_i_real_tvalid : STD_LOGIC;
  signal wireToFir0_i_real_tready : STD_LOGIC;
  
  -- FIR 0 to 1
  signal wireFir0ToFir1_i_real_tdata : STD_LOGIC_VECTOR ( 47 downto 0 );
  signal wireFir0ToFir1_i_real_tvalid : STD_LOGIC;
  
  -- Wires out of FIR 1
  signal wireToFir1_i_real_tready : STD_LOGIC;
  signal wireFromFir1_i_real_tdata : STD_LOGIC_VECTOR (71 DOWNTO 0) := (others=>'0');
  signal wireFromFir1_i_real_tvalid : STD_LOGIC;
  
  
  -- output --------------------------------------------------------------------
  signal m_axis_tvalid_internal : STD_LOGIC;
  signal m_axis_tvalid_internal_real : STD_LOGIC;
  signal m_axis_tvalid_internal_im : STD_LOGIC;

begin
    
    
    --TODO: UPDATE THE RESETS!!!!
    
    -- GPIO to Fake ADC (DDS Compiler)
    wireValidPhase <= '1';
--    i_ddsReset <= '0' when slv_reg2 = '0x00000001' else '0';
--    i_ddsReset <= '1';
    
    -- GPIO to Tuner (DDS Compiler)
    wireValidPhaseTuner <= '1';
--    i_ddsResetTuner <= '1';
--------------------------------------------------------------------------------
 
    -- To FIR 0 imaginary
    wireToFir0_q_imaginary_tdata <= output_q_32(31 downto 16);--(15 downto 0); --T0D0
    wireToFir0_q_imaginary_tvalid <= '1';
    
    -- To FIR 0 real
    wireToFir0_i_real_tdata <= output_i_32(31 downto 16);--(15 downto 0);
    wireToFir0_i_real_tvalid <= '1';    
--------------------------------------------------------------------------------
    -- Complex multiplication
    -- (a + jb)(c + jd) = (ac - bd) + j(ad + bc)
    -- Note that the Fake ADC only has a real component (imaginary part is "zero")
    --     Output_i <= dds_cos * adc_data?
    --     Output_q <= dds_sin * adc_data
    
    -- Transition from std_logic_vector to integer
    int_fakeADC <= TO_INTEGER(signed(i_ddsFreq_tdata));
    int_tuner_dds_cos <= TO_INTEGER(signed(dds_cos));
    int_tuner_dds_sin <= TO_INTEGER(signed(dds_sin)); 
    
    -- Multiply integers
    int_i_real_output <= int_tuner_dds_cos*int_fakeADC;
    int_q_imaginary_output <= int_tuner_dds_sin*int_fakeADC;
    
    -- Transition from integer to std_logic_vector
    output_i_32 <= std_logic_vector(TO_SIGNED(int_i_real_output,output_i_32'length));
    output_q_32 <= std_logic_vector(TO_SIGNED(int_q_imaginary_output,output_q_32'length));
      
--------------------------------------------------------------------------------
    -- OUTPUTS
    m_axis_tdata(15 downto 0)  <= wireFromFir1_q_imaginary_tdata(64 downto 49);
    m_axis_tdata(31 downto 16)  <= wireFromFir1_i_real_tdata(64 downto 49);
    --Ensure both the real and imaginary are valid
    m_axis_tvalid <= m_axis_tvalid_internal_im AND m_axis_tvalid_internal_real;
--------------------------------------------------------------------------------


	-- I/O Connections assignments

	S_AXI_AWREADY	<= axi_awready;
	S_AXI_WREADY	<= axi_wready;
	S_AXI_BRESP	<= axi_bresp;
	S_AXI_BVALID	<= axi_bvalid;
	S_AXI_ARREADY	<= axi_arready;
	S_AXI_RDATA	<= axi_rdata;
	S_AXI_RRESP	<= axi_rresp;
	S_AXI_RVALID	<= axi_rvalid;
	-- Implement axi_awready generation
	-- axi_awready is asserted for one S_AXI_ACLK clock cycle when both
	-- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_awready is
	-- de-asserted when reset is low.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_awready <= '0';
	      aw_en <= '1';
	    else
	      if (axi_awready = '0' and S_AXI_AWVALID = '1' and S_AXI_WVALID = '1' and aw_en = '1') then
	        -- slave is ready to accept write address when
	        -- there is a valid write address and write data
	        -- on the write address and data bus. This design 
	        -- expects no outstanding transactions. 
	           axi_awready <= '1';
	           aw_en <= '0';
	        elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then
	           aw_en <= '1';
	           axi_awready <= '0';
	      else
	        axi_awready <= '0';
	      end if;
	    end if;
	  end if;
	end process;

	-- Implement axi_awaddr latching
	-- This process is used to latch the address when both 
	-- S_AXI_AWVALID and S_AXI_WVALID are valid. 

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_awaddr <= (others => '0');
	    else
	      if (axi_awready = '0' and S_AXI_AWVALID = '1' and S_AXI_WVALID = '1' and aw_en = '1') then
	        -- Write Address latching
	        axi_awaddr <= S_AXI_AWADDR;
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_wready generation
	-- axi_wready is asserted for one S_AXI_ACLK clock cycle when both
	-- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_wready is 
	-- de-asserted when reset is low. 

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_wready <= '0';
	    else
	      if (axi_wready = '0' and S_AXI_WVALID = '1' and S_AXI_AWVALID = '1' and aw_en = '1') then
	          -- slave is ready to accept write data when 
	          -- there is a valid write address and write data
	          -- on the write address and data bus. This design 
	          -- expects no outstanding transactions.           
	          axi_wready <= '1';
	      else
	        axi_wready <= '0';
	      end if;
	    end if;
	  end if;
	end process; 

	-- Implement memory mapped register select and write logic generation
	-- The write data is accepted and written to memory mapped registers when
	-- axi_awready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted. Write strobes are used to
	-- select byte enables of slave registers while writing.
	-- These registers are cleared when reset (active low) is applied.
	-- Slave register write enable is asserted when valid address and data are available
	-- and the slave is ready to accept the write address and write data.
	slv_reg_wren <= axi_wready and S_AXI_WVALID and axi_awready and S_AXI_AWVALID ;

	process (S_AXI_ACLK)
	variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0); 
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      slv_reg0 <= (others => '0');
	      slv_reg1 <= (others => '0');
	      slv_reg2 <= (others => '0');
	      slv_reg3 <= (others => '0');
	    else
	      loc_addr := axi_awaddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
	      if (slv_reg_wren = '1') then
	        case loc_addr is
	          when b"00" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 0
	                slv_reg0(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"01" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 1
	                slv_reg1(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"10" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 2
	                slv_reg2(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when b"11" =>
	            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
	              if ( S_AXI_WSTRB(byte_index) = '1' ) then
	                -- Respective byte enables are asserted as per write strobes                   
	                -- slave registor 3
	                slv_reg3(byte_index*8+7 downto byte_index*8) <= S_AXI_WDATA(byte_index*8+7 downto byte_index*8);
	              end if;
	            end loop;
	          when others =>
	            slv_reg0 <= slv_reg0;
	            slv_reg1 <= slv_reg1;
	            slv_reg2 <= slv_reg2;
	            slv_reg3 <= slv_reg3;
	        end case;
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement write response logic generation
	-- The write response and response valid signals are asserted by the slave 
	-- when axi_wready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted.  
	-- This marks the acceptance of address and indicates the status of 
	-- write transaction.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_bvalid  <= '0';
	      axi_bresp   <= "00"; --need to work more on the responses
	    else
	      if (axi_awready = '1' and S_AXI_AWVALID = '1' and axi_wready = '1' and S_AXI_WVALID = '1' and axi_bvalid = '0'  ) then
	        axi_bvalid <= '1';
	        axi_bresp  <= "00"; 
	      elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then   --check if bready is asserted while bvalid is high)
	        axi_bvalid <= '0';                                 -- (there is a possibility that bready is always asserted high)
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_arready generation
	-- axi_arready is asserted for one S_AXI_ACLK clock cycle when
	-- S_AXI_ARVALID is asserted. axi_awready is 
	-- de-asserted when reset (active low) is asserted. 
	-- The read address is also latched when S_AXI_ARVALID is 
	-- asserted. axi_araddr is reset to zero on reset assertion.

	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
	      axi_arready <= '0';
	      axi_araddr  <= (others => '1');
	    else
	      if (axi_arready = '0' and S_AXI_ARVALID = '1') then
	        -- indicates that the slave has acceped the valid read address
	        axi_arready <= '1';
	        -- Read Address latching 
	        axi_araddr  <= S_AXI_ARADDR;           
	      else
	        axi_arready <= '0';
	      end if;
	    end if;
	  end if;                   
	end process; 

	-- Implement axi_arvalid generation
	-- axi_rvalid is asserted for one S_AXI_ACLK clock cycle when both 
	-- S_AXI_ARVALID and axi_arready are asserted. The slave registers 
	-- data are available on the axi_rdata bus at this instance. The 
	-- assertion of axi_rvalid marks the validity of read data on the 
	-- bus and axi_rresp indicates the status of read transaction.axi_rvalid 
	-- is deasserted on reset (active low). axi_rresp and axi_rdata are 
	-- cleared to zero on reset (active low).  
	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then
	    if S_AXI_ARESETN = '0' then
	      axi_rvalid <= '0';
	      axi_rresp  <= "00";
	    else
	      if (axi_arready = '1' and S_AXI_ARVALID = '1' and axi_rvalid = '0') then
	        -- Valid read data is available at the read data bus
	        axi_rvalid <= '1';
	        axi_rresp  <= "00"; -- 'OKAY' response
	      elsif (axi_rvalid = '1' and S_AXI_RREADY = '1') then
	        -- Read data is accepted by the master
	        axi_rvalid <= '0';
	      end if;            
	    end if;
	  end if;
	end process;

	-- Implement memory mapped register select and read logic generation
	-- Slave register read enable is asserted when valid address is available
	-- and the slave is ready to accept the read address.
	slv_reg_rden <= axi_arready and S_AXI_ARVALID and (not axi_rvalid) ;

	process (slv_reg0, slv_reg1, slv_reg2, slv_reg3, axi_araddr, S_AXI_ARESETN, slv_reg_rden)
	variable loc_addr :std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
	begin
	    -- Address decoding for reading registers
	    loc_addr := axi_araddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
	    case loc_addr is
	      when b"00" =>
	        reg_data_out <= slv_reg0; --Fake ADC (DDS)
	      when b"01" =>
	        reg_data_out <= slv_reg1; --Tuner DDS 
	      when b"10" =>
	        reg_data_out <= slv_reg2; --Time of reset --x"BE49640B";--slv_reg2;
	      when b"11" =>
	        reg_data_out <= timeSinceReset;--slv_reg3; --Counter
	      when others =>
	        reg_data_out  <= (others => '0');
	    end case;
	end process; 

	-- Output register or memory read data
	process( S_AXI_ACLK ) is
	begin
	  if (rising_edge (S_AXI_ACLK)) then
	    if ( S_AXI_ARESETN = '0' ) then
	      axi_rdata  <= (others => '0');
	    else
	      if (slv_reg_rden = '1') then
	        -- When there is a valid read address (S_AXI_ARVALID) with 
	        -- acceptance of read address by the slave (axi_arready), 
	        -- output the read dada 
	        -- Read address mux
	          axi_rdata <= reg_data_out;     -- register read data
	      end if;   
	    end if;
	  end if;
	end process;


	-- Add user logic here
	process (S_AXI_ACLK)
	begin
	  if rising_edge(S_AXI_ACLK) then 
	    if S_AXI_ARESETN = '0' then
--	      axi_wready <= '0';
	    else
	     if (slv_reg2 = "00000001") then
	      i_ddsReset <= '0';
	      timeSinceReset <= (others=>'0');
	     else
	      i_ddsReset <= '1';
	      timeSinceReset <= std_logic_vector(to_unsigned(to_integer(unsigned( timeSinceReset )) + 1, 32)); --timeSinceReset + '1';
	     end if;
	      
	     -- i_ddsReset <= '0' when slv_reg2 = '0x00000001' else '0';
--	      if (axi_wready = '0' and S_AXI_WVALID = '1' and S_AXI_AWVALID = '1' and aw_en = '1') then
--	          -- slave is ready to accept write data when 
--	          -- there is a valid write address and write data
--	          -- on the write address and data bus. This design 
--	          -- expects no outstanding transactions.           
--	          axi_wready <= '1';
--	      else
--	        axi_wready <= '0';
--	      end if;
	    end if;
	  end if;
	end process; 
	
--DDS Compiler start

--Fake ADC (DDS Compiler)
fake_ADC_i: component dds_compiler
     port map (
      --Use the same clock and reset as DACIF
      aclk => s_axi_aclk,
      aresetn => resetn_ddsc_sysclk,--TODO UPDATE
      
      -- Input from gpio - Phase Increment
      s_axis_phase_tdata(31 downto 0) => slv_reg0,
      s_axis_phase_tvalid => wireValidPhase,
      
      --Connect to Complex Multiplier
      m_axis_data_tdata(15 downto 0) => i_ddsFreq_tdata(15 downto 0),
      m_axis_data_tvalid => i_ddsFreq_tvalid
    );
    
--Tuner
tunerDds_i: component tunerDds
     port map (
      --Use the same clock and reset as DACIF
      aclk => s_axi_aclk,
      aresetn => resetn_ddsc_sysclkTuner,--TODO UPDATE
      
      -- Input from gpio - Phase Increment
      s_axis_phase_tdata(31 downto 0) => slv_reg1,
      s_axis_phase_tvalid => wireValidPhaseTuner,
      
      
      --Connect to Complex Multiplier
      m_axis_data_tdata(31 downto 16) => dds_sin, --sine
      m_axis_data_tdata(15 downto 0) => dds_cos, --cosine
      m_axis_data_tvalid => i_ddsFreq_tvalidTuner
    );
--DDS Compiler start



--Filters
--FIR 0 - Imaginary
fir_compiler_0_imaginary : fir_compiler_0
  PORT MAP (
    aclk => s_axi_aclk,
    
    -- From Complex Multiplier
    s_axis_data_tvalid => wireToFir0_q_imaginary_tvalid,
    s_axis_data_tdata => wireToFir0_q_imaginary_tdata,
    
    -- Disconnected
    s_axis_data_tready => wireToFir0_q_imaginary_tready,
    
    --FIR 0 to 1
    m_axis_data_tvalid => wireFir0ToFir1_q_imaginary_tvalid,
    m_axis_data_tdata => wireFir0ToFir1_q_imaginary_tdata
  );
--FIR 1 - Imaginary
fir_compiler_1_imaginary : fir_compiler_1
  PORT MAP (
    aclk => s_axi_aclk,
    
    --FIR 0 to 1
    s_axis_data_tvalid => wireFir0ToFir1_q_imaginary_tvalid,
    s_axis_data_tdata => wireFir0ToFir1_q_imaginary_tdata,
    
    s_axis_data_tready => wireToFir1_q_imaginary_tready,
    
    m_axis_data_tvalid => m_axis_tvalid_internal_im,
    m_axis_data_tdata => wireFromFir1_q_imaginary_tdata
  );

--FIR 0 - Real
fir_compiler_0_real : fir_compiler_0
  PORT MAP (
    aclk => s_axi_aclk,
    
    -- From Complex Multiplier
    s_axis_data_tvalid => wireToFir0_i_real_tvalid,
    s_axis_data_tdata => wireToFir0_i_real_tdata,
    
    -- Disconnected
    s_axis_data_tready => wireToFir0_i_real_tready,
    
    --FIR 0 to 1
    m_axis_data_tvalid => wireFir0ToFir1_i_real_tvalid,
    m_axis_data_tdata => wireFir0ToFir1_i_real_tdata
  );
--FIR 1 - Real
fir_compiler_1_real : fir_compiler_1
  PORT MAP (
    aclk => s_axi_aclk,
    
    --FIR 0 to 1
    s_axis_data_tvalid => wireFir0ToFir1_i_real_tvalid,
    s_axis_data_tready => wireToFir1_i_real_tready,
    s_axis_data_tdata => wireFir0ToFir1_i_real_tdata,
    
    m_axis_data_tvalid => m_axis_tvalid_internal_real,
    m_axis_data_tdata => wireFromFir1_i_real_tdata
  );


   xpm_cdc_async_rst_inst_dds_c_fakeADC : xpm_cdc_async_rst
   generic map (
      DEST_SYNC_FF => 4,    -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,    -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      RST_ACTIVE_HIGH => 0  -- DECIMAL; 0=active low reset, 1=active high reset
   )
   port map (
      dest_arst => resetn_ddsc_sysclk, -- 1-bit output: src_arst asynchronous reset signal synchronized to destination
                              -- clock domain. This output is registered. NOTE: Signal asserts asynchronously
                              -- but deasserts synchronously to dest_clk. Width of the reset signal is at least
                              -- (DEST_SYNC_FF*dest_clk) period.
      dest_clk => s_axi_aclk,   -- 1-bit input: Destination clock.
      src_arst => i_ddsReset    -- 1-bit input: Source asynchronous reset signal.
   );
   
   xpm_cdc_async_rst_inst_dds_c_Tuner : xpm_cdc_async_rst
   generic map (
      DEST_SYNC_FF => 4,    -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,    -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      RST_ACTIVE_HIGH => 0  -- DECIMAL; 0=active low reset, 1=active high reset
   )
   port map (
      dest_arst => resetn_ddsc_sysclkTuner, -- 1-bit output: src_arst asynchronous reset signal synchronized to destination
                              -- clock domain. This output is registered. NOTE: Signal asserts asynchronously
                              -- but deasserts synchronously to dest_clk. Width of the reset signal is at least
                              -- (DEST_SYNC_FF*dest_clk) period.
      dest_clk => s_axi_aclk,   -- 1-bit input: Destination clock.
      src_arst => i_ddsResetTuner    -- 1-bit input: Source asynchronous reset signal.
   );

	-- User logic ends

end arch_imp;
