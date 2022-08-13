---
title: "Getting started with verilog"
slug: "getting-started-with-verilog"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
Verilog is a hardware description language (HDL) used to model electronic systems. It most commonly describes an electronic system at the register-transfer level (RTL) of abstraction. It is also used in the verification of analog circuits and mixed-signal circuits. Its structure and main principles ( as described below ) are designed to describe and successfully implement an electronic system.

 - Rigidity   
 An electronic circuit is a physical entity having a fixed structure and Verilog is adapted for that. Modules (module), Ports(input/output/inout) , connections (wires), blocks (@always), registers (reg) are all fixed at compile time. The number of entities and interconnects do not change dynamically. There is always a "module" at the top-level representing the chip structure (for synthesis), and one at the system level for verification.
 - Parallelism     
The inherent simultaneous operations in the physical chip is mimicked in the language by always (most commmon), initial and fork/join blocks.
   

      module top();
      reg r1,r2,r3,r4; // 1-bit registers
        initial
        begin
          r1 <= 0 ;
        end
        initial
        begin
          fork
             r2 <= 0 ;
             r3 <= 0 ;
          join
        end
         always @(r4)
            r4 <= 0 ;
     endmodule

   All of the above statements are executed in parallel within the same time unit.

 - Timing and Synchronization    
Verilog supports various constructs to describe the temporal nature of circuits. Timings and delays in circuits can be implemented in Verilog, for example by #delay constructs. Similarly, Verilog also accommodates for synchronous and asynchronous circuits and components like flops, latches and combinatorial logic using various constructs, for example "always" blocks. A set of blocks can also be synchronized via a common clock signal or a block can be triggered based on specific set of inputs. 

       #10   ;                 // delay for 10 time units
       always @(posedge clk )  // synchronous 
       always @(sig1 or sig2 ) // combinatorial logic  
       @(posedge event1)       // wait for post edge transition of event1
       wait (signal  == 1)     // wait for signal to be 1

   
 - Uncertainty    
Verilog supports some of the uncertainty inherent in electronic circuits. "X" is used to represent unknown state of the circuit. "Z" is used to represent undriven state of the circuit.

       reg1 = 1'bx;
       reg2 = 1'bz;

 - Abstraction    
Verilog supports designing at different levels of abstraction. The highest level of abstraction for a design is the Resister transfer Level (RTL), the next being the gate level and the lowest the cell level ( User Define Primitives ), RTL abstraction being the most commonly used. Verilog also supports behavioral level of abstraction with no regard to the structural realization of the design, primarily used for verification.

 

     // Example of a D flip flop at RTL abstraction
    module dff (
     clk    , // Clock Input
     reset  , // Reset input
     d       , // Data Input
     q        // Q output
     );
     //-----------Input Ports---------------
     input d, clk, reset ;
    
     //-----------Output Ports---------------
     output q;
    
     reg q;
    
     always @ ( posedge clk)
     if (~reset) begin
       q <= 1'b0;
     end  else begin
       q <= d;
     end
    
    endmodule


    // And gate model based at Gate level abstraction 
    module and(input x,input y,output o);
    
    wire  w;
    // Two instantiations of the module NAND
    nand U1(w,x, y); 
    nand U2(o, w, w); 
    
    endmodule

    // Gate modeled at Cell-level Abstraction
    primitive udp_and(
    a, // declare three ports
    b,
    c 
    );
    output a;   // Outputs
    input b,c;  // Inputs 
    
    // UDP function code here
    // A = B & C;
    table
     // B  C    : A 
        1  1    : 1;
        0  1    : 0;
        1  0    : 0;
        0  0    : 0;
    endtable
    
    endprimitive



----------



There are three main use cases for Verilog. They determine the structure of the code and its interpretation and also determine the tool sets used. All three applications are necessary for successful implementation of any Verilog design.

 1. Physical Design / Back-end   
 Here Verilog is used to primarily view the design as a matrix of interconnecting gates implementing logical design. RTL/logic/Design goes through various steps from synthesis -> placement -> clock tree construction -> routing -> DRC -> LVS -> to tapeout. The precise steps and sequences vary based on the exact nature of implementation.
 2. Simulation    
 In this use case, the primary aim is to generate test vectors to validate the design as per the specification. The code written in this use case need not be synthesizable and it remains within verification sphere. The code here more closely resembles generic software structures like for/while/do loops etc.  
 3. Design   
Design involves implementing the specification of a circuit generally at the RTL level of abstraction. The Verilog code is then given for Verification and the fully verified code is given for physical implementation. The code is written using only the synthesizable constructs of Verilog. Certain RTL coding style can cause simulation vs synthesis mismatch and care has to be taken to avoid those. 


----------


 There are two main implementation flows. They will also affect the way Verilog code is written and implemented. Certain styles of coding and certain structures are more suitable in one flow over the other.

   - ASIC Flow (application-specific integrated circuit)
   - FPGA Flow (Field-programmable gate array) - include FPGA and CPLD's

 

## Hello World
This example uses the icarus verilog compiler.  

Step 1: Create a file called hello.v

    module myModule();
    
    initial
      begin
        $display("Hello World!");   // This will display a message
        $finish ; // This causes the simulation to end.  Without, it would go on..and on.
      end
    
    endmodule

Step 2.  We compile the .v file using icarus:

    >iverilog -o hello.vvp hello.v

The -o switch assigns a name to the output object file.  Without this switch the output file would be called a.out.
The hello.v indicates the source file to be compiled.  There should be practically no output when you compile this source code, unless there are errors.

Step 3. You are ready to simulate this Hello World verilog program.  To do so, invoke as such:

    >vvp hello.vvp 
    Hello World!
    >



## Installation of Icarus Verilog Compiler for Mac OSX Sierra
 1. Install Xcode from the App Store.
 2. Install the Xcode developer tools
 

    > xcode-select --install

This will provide basic command line tools such as `gcc` and `make`

 3. Install Mac Ports
`https://www.macports.org/install.php`

The OSX Sierra install package will provide an open-source method of installing and upgrading additional software packages on the Mac platform.  Think `yum` or `apt-get` for the Mac.

 4. Install icarus using Mac ports


    > sudo port install iverilog


 5. Verify the installation from the command line


    $ iverilog
    iverilog: no source files.
    
    Usage: iverilog [-ESvV] [-B base] [-c cmdfile|-f cmdfile]
                    [-g1995|-g2001|-g2005] [-g<feature>]
                    [-D macro[=defn]] [-I includedir] [-M depfile] [-m module]
                    [-N file] [-o filename] [-p flag=value]
                    [-s topmodule] [-t target] [-T min|typ|max]
                    [-W class] [-y dir] [-Y suf] source_file(s)
    
    See the man page for details.
    $
You are now ready to compile and simulate your first Verilog file on the Mac.

## Install GTKWave for graphical display of simulation data on Mac OSx Sierra
GTKWave is a fully feature graphical viewing package that supports several graphical data storage standards, but it also happens to support VCD, which is the format that `vvp` will output.  So, to pick up GTKWave, you have a couple options
1. Goto http://gtkwave.sourceforge.net/gtkwave.zip and download it.  This version is typically the latest.
2. If you have installed MacPorts (<https://www.macports.org/>), simply run `sudo port install gtkwave`.  This will probably want to install on the dependencies to.  Note this method will usually get you an older version.  If you don't have MacPorts installed, there is a installation setup example for doing this on this page.  Yes! You will need all of the xcode developer tools, as this methods will "build" you a GTKWave from source.

When installation is done, you may be asked to select a python version. I already had 2.7.10 installed so I never "selected" a new one.

At this point you can start gtkwave from the command line with `gtkwave`.  When it starts you may be asked to install, or update XQuarts.  Do so.  In my case XQuarts 2.7.11 is installed.

Note: I actually needed to reboot to get XQuarts correctly, then I typed `gtkwave` again and the application comes up.  

In the next example, I will create two independent files, a testbench and module to test, and we will use the gtkwave to view the design.







## Using Icarus Verilog and GTKWaves to simulate and view a design graphically
This example uses Icarus and GTKWave.  Installation instructions for those tools on OSx are provided elsewhere on this page. 

Lets begin with the module design.  This module is a BCD to 7 segment display.  I have coded the design in an obtuse way simply to give us something that is easily broken and we can spend sometime fixing graphically.  So we have a clock, reset, a 4 data input representing a BCD value, and a 7 bit output that represent the seven segment display. Create a file called bcd_to_7seg.v and place the source below in it.

    module bcd_to_7seg (
       input clk,
       input reset,
       input [3:0] bcd,
       output [6:0] seven_seg_display
    
    );
       parameter TP = 1;
       reg seg_a;
       reg seg_b;
       reg seg_c;
       reg seg_d;
       reg seg_e;
       reg seg_f;
       reg seg_g;
    
       
       always @ (posedge clk or posedge reset)
          begin
          if (reset)
             begin
                seg_a <= #TP 1'b0;
                seg_b <= #TP 1'b0;
                seg_c <= #TP 1'b0;
                seg_d <= #TP 1'b0;
                seg_e <= #TP 1'b0;
                seg_f <= #TP 1'b0;
                seg_g <= #TP 1'b0;
             end
          else
             begin
                seg_a <= #TP  ~(bcd == 4'h1 || bcd == 4'h4);
                seg_b <= #TP  bcd < 4'h5 || bcd > 6;
                seg_c <= #TP   bcd != 2;
                seg_d <= #TP   bcd == 0 || bcd[3:1] == 3'b001 || bcd == 5 || bcd == 6 || bcd == 8;
                seg_e <= #TP  bcd == 0 || bcd == 2 || bcd == 6 || bcd == 8;
                seg_f <= #TP  bcd == 0 || bcd == 4 || bcd == 5 || bcd == 6 || bcd > 7;
                seg_g <= #TP  (bcd > 1 && bcd < 7) || (bcd > 7);
             end
        end
     
        assign seven_seg_display = {seg_g,seg_f,seg_e,seg_d,seg_c,seg_b,seg_a};
    endmodule

Next, we need a test to check if this module is working correctly.  The case statement in the testbench is actually easier to read in my opinion and more clear as to what it does.  But I did not wan to put the same case statment in the design AND in the test.  That is bad practice.  Rather two independent designs are being used to validate one-another.

Withing the code below, you will notice two lines
`$dumpfile("testbench.vcd");` and `$dumpvars(0,testbench);`.  These lines are what create the VCD output file that will be used to perform graphical analysis of the design.  If you leave them out, you won't get a VCD file generated.  Create a file called testbench.v and place the source below in it.


    `timescale 1ns/100ps
    module testbench;
    reg clk;
    reg reset;
    reg [31:0] ii;
    reg [31:0] error_count;
    reg [3:0] bcd;
    wire [6:0] seven_seg_display; 
    parameter TP = 1;
    parameter CLK_HALF_PERIOD = 5;
     
       // assign clk = #CLK_HALF_PERIOD ~clk;  // Create a clock with a period of ten ns
       initial
       begin
         clk = 0;
         #5;
         forever clk = #( CLK_HALF_PERIOD )  ~clk;
       end
    
       initial
         begin
           $dumpfile("testbench.vcd");
           $dumpvars(0,testbench);
           // clk  = #CLK_HALF_PERIOD ~clk; 
           $display("%0t, Reseting system", $time);
           error_count = 0;
           bcd  = 4'h0;
           reset = #TP 1'b1;
           repeat (30) @ (posedge clk);
           reset  = #TP 1'b0;
           repeat (30) @ (posedge clk);
           $display("%0t, Begin BCD test", $time); // This displays a message
    
    
           for (ii = 0; ii < 10; ii = ii + 1)
              begin
              repeat (1) @ (posedge clk);
              bcd  = ii[3:0];
              repeat (1) @ (posedge clk); 
              if (seven_seg_display !== seven_seg_prediction(bcd)) 
                 begin
                    $display("%0t, ERROR: For BCD %d, module output 0b%07b does not match prediction logic value of 0b%07b.",$time,bcd, seven_seg_display,seven_seg_prediction(bcd));
                    error_count = error_count + 1;
                 end
              end
           $display("%0t, Test Complete with %d errors", $time, error_count);
           $display("%0t, Test %s", $time, ~|error_count ? "pass." : "fail.");
           $finish ; // This causes the simulation to end.
         end
    
    
    parameter SEG_A = 7'b0000001;
    parameter SEG_B = 7'b0000010;
    parameter SEG_C = 7'b0000100;
    parameter SEG_D = 7'b0001000;
    parameter SEG_E = 7'b0010000;
    parameter SEG_F = 7'b0100000;
    parameter SEG_G = 7'b1000000;
    
    function [6:0] seven_seg_prediction;
       input [3:0] bcd_in;
    
       //    +--- A ---+
       //    |         |
       //    F         B
       //    |         |
       //    +--- G ---+
       //    |         |
       //    E         C
       //    |         |
       //    +--- D ---+
    
       begin
          case (bcd_in)
             4'h0: seven_seg_prediction = SEG_A | SEG_B | SEG_C | SEG_D | SEG_E | SEG_F;
             4'h1: seven_seg_prediction = SEG_B | SEG_C;
             4'h2: seven_seg_prediction = SEG_A | SEG_B | SEG_G | SEG_E | SEG_D;
             4'h3: seven_seg_prediction = SEG_A | SEG_B | SEG_G | SEG_C | SEG_D;
             4'h4: seven_seg_prediction = SEG_F | SEG_G | SEG_B | SEG_C;
             4'h5: seven_seg_prediction = SEG_A | SEG_F | SEG_G | SEG_C | SEG_D;
             4'h6: seven_seg_prediction = SEG_A | SEG_F | SEG_G | SEG_E | SEG_C | SEG_D;
             4'h7: seven_seg_prediction = SEG_A | SEG_B | SEG_C;
             4'h8: seven_seg_prediction = SEG_A | SEG_B | SEG_C | SEG_D | SEG_E | SEG_F | SEG_G;
             4'h9: seven_seg_prediction = SEG_A | SEG_F | SEG_G | SEG_B | SEG_C;
             default: seven_seg_prediction = 7'h0;
          endcase
       end
    endfunction
    
    
    bcd_to_7seg u0_bcd_to_7seg (
    .clk               (clk),
    .reset             (reset),
    .bcd               (bcd),
    .seven_seg_display (seven_seg_display)
    );
    
    
    endmodule

Now that we have two files, a testbench.v and bcd_to_7seg.v, we need to compile, elaborate using Icarus.  To do this:

    $ iverilog -o testbench.vvp testbench.v bcd_to_7seg.v

Next we need to simulate

    $ vvp testbench.vvp 
    LXT2 info: dumpfile testbench.vcd opened for output.
    0, Reseting system
    6000, Begin BCD test
    8000, Test Complete with          0 errors
    8000, Test pass. 

At this point if you want to validate the file is really being tested, go into the bcd_2_7seg.v file and move some of the logic around and repeat those first two steps.

As an example I change the line ` seg_c <= #TP   bcd != 2;` to ` seg_c <= #TP   bcd != 4;`.  Recompile and simulate does the following:

    $ iverilog -o testbench.vvp testbench.v bcd_to_7seg.v
    $ vvp testbench.vvp 
    LXT2 info: dumpfile testbench.vcd opened for output.
    0, Reseting system
    6000, Begin BCD test
    6600, ERROR: For BCD  2, module output 0b1011111 does not match prediction logic value of 0b1011011.
    7000, ERROR: For BCD  4, module output 0b1100010 does not match prediction logic value of 0b1100110.
    8000, Test Complete with          2 errors
    8000, Test fail.
    $

So now, lets view the simulation using GTKWave.  From the command line, issue a

 `gtkwave testbench.vcd &`

When the GTKWave window appears, you will see in the upper left hand box, the module name testbench.  Click it.  This will reveal the sub-modules, tasks, and functions associated with that file.  Wires and registers will also appear in the lower left hand box. 

Now drag, clk, bcd, error_count and seven_seg_display into the signal box next to the waveform window.  The signals will now be plotted.  Error_count will show you which particular BCD input generated the wrong seven_seg_display output.

You are now ready to troubleshoot a Verilog bug graphically. 


## Installation or Setup
Detailed instructions on getting Verilog set up or installed is dependent on the tool you use since there are many Verilog tools.

