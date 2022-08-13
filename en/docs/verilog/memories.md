---
title: "Memories"
slug: "memories"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

For FIFOs, you typically instantiate a vendor-specific block (also called a "core" or "IP").

## Simple Dual Port RAM
Simple Dual Port RAM with separate addresses and clocks for read/write operations.

    module simple_ram_dual_clock #(
      parameter DATA_WIDTH=8,                 //width of data bus
      parameter ADDR_WIDTH=8                  //width of addresses buses
    )(
      input      [DATA_WIDTH-1:0] data,       //data to be written
      input      [ADDR_WIDTH-1:0] read_addr,  //address for read operation
      input      [ADDR_WIDTH-1:0] write_addr, //address for write operation
      input                       we,         //write enable signal
      input                       read_clk,   //clock signal for read operation
      input                       write_clk,  //clock signal for write operation
      output reg [DATA_WIDTH-1:0] q           //read data
    );
        
      reg [DATA_WIDTH-1:0] ram [2**ADDR_WIDTH-1:0]; // ** is exponentiation
        
      always @(posedge write_clk) begin //WRITE
        if (we) begin 
          ram[write_addr] <= data;
        end
      end
        
      always @(posedge read_clk) begin //READ
        q <= ram[read_addr];
      end
        
    endmodule



## Shift register
N-bit deep shift register with asynchronous reset.

    module shift_register #(
      parameter REG_DEPTH = 16
    )(
      input clk,       //clock signal
      input ena,       //enable signal
      input rst,       //reset signal
      input data_in,   //input bit
      output data_out  //output bit
    );
    
      reg [REG_DEPTH-1:0] data_reg;
    
      always @(posedge clk or posedge rst) begin
        if (rst) begin //asynchronous reset
          data_reg <= {REG_DEPTH{1'b0}}; //load REG_DEPTH zeros
        end else if (enable) begin
          data_reg <= {data_reg[REG_DEPTH-2:0], data_in}; //load input data as LSB and shift (left) all other bits 
        end
      end
    
      assign data_out = data_reg[REG_DEPTH-1]; //MSB is an output bit
    
    endmodule



## Single Port Synchronous RAM
Simple Single Port RAM with one address for read/write operations.

    module ram_single #(
      parameter DATA_WIDTH=8,          //width of data bus
      parameter ADDR_WIDTH=8           //width of addresses buses
    )(
      input  [(DATA_WIDTH-1):0] data,  //data to be written
      input  [(ADDR_WIDTH-1):0] addr,  //address for write/read operation
      input                     we,    //write enable signal
      input                     clk,   //clock signal
      output [(DATA_WIDTH-1):0] q      //read data
    );
    
      reg [DATA_WIDTH-1:0] ram [2**ADDR_WIDTH-1:0];
      reg [ADDR_WIDTH-1:0] addr_r;
    
      always @(posedge clk) begin //WRITE
          if (we) begin
              ram[addr] <= data;
          end
          addr_r <= addr;
      end
    
      assign q = ram[addr_r]; //READ
    
    endmodule



## Single Port Async Read/Write RAM
Simple single port RAM with async read/write operations

    module ram_single_port_ar_aw #(
      parameter DATA_WIDTH = 8,
      parameter ADDR_WITDH = 3
    )(
      input                       we,    // write enable
      input                       oe,    // output enable
      input  [(ADDR_WITDH-1):0]   waddr, // write address
      input  [(DATA_WIDTH-1):0]   wdata, // write data
      input                       raddr, // read adddress
      output [(DATA_WIDTH-1):0]   rdata  // read data
    );

      reg [(DATA_WIDTH-1):0]      ram [0:2**ADDR_WITDH-1];
      reg [(DATA_WIDTH-1):0]      data_out;

      assign rdata = (oe && !we) ? data_out : {DATA_WIDTH{1'bz}};

      always @*
      begin : mem_write
        if (we) begin
          ram[waddr] = wdata;
        end
      end

      always @* // if anything below changes (i.e. we, oe, raddr), execute this    
      begin : mem_read
        if (!we && oe) begin
          data_out = ram[raddr];
        end
      end

    endmodule


