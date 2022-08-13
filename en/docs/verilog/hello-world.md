---
title: "Hello World"
slug: "hello-world"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Compiling and Running the Example
Assuming a source file of `hello_world.v` and a top level module of `hello_world`. The code can be run using various simulators. Most simulators are compiled simulators. They require multiple steps to compile and execute. 
Generally the

 - First step is to compile the Verilog design.
 - Second step is to elaborate and optimize the design.
 - Third step is to run the simulation.

The details of the steps could vary based on the simulator but the overall idea remains the same.

Three step process using Cadence Simulator

     ncvlog hello_world.v
     ncelab hello_world
     ncsim hello_world

 - First step ncvlog is to compile the file hello_world.v    
 - Second step ncelab is to elaborate the code with the top level module hello_world. 
 - Third step ncsim is to run the simulation with the top level module hello_world.
 - The simulator generates all the compiled and optimized code into a work lib. [ INCA_libs - default library name ] 

single step using Cadence Simulator. 

The command line will internally call the required three steps. This is to mimic the older interpreted simulator execution style ( single command line ). 

    irun hello_world.v   
    or 
    ncverilog hello_world.v

## Hello World
The program outputs Hello World! to standard output.

    module HELLO_WORLD(); // module doesn't have input or outputs
      initial begin
        $display("Hello World");
        $finish; // stop the simulator
      end
    endmodule

Module is a basic building block in Verilog. It represent a collection of elements and is enclosed between module and end module keyword. Here hello_world is the top most (and the only) module . 

Initial block executes at the start of simulation. The begin and end is used to mark the boundary of the initial block. `$display` outputs the message to the standard output. It inserts and end of line "\n" to the message.

This code can't by synthesized, i.e. it can't be put in a chip.     




