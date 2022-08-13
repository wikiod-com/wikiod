---
title: "Getting started with system-verilog"
slug: "getting-started-with-system-verilog"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
In order to compile and run SystemVerilog code a tool called a simulator is needed. Most commonly, commercial tools from one of the Big Three EDA companies is used:

 - Cadence Incisive
 - Mentor Graphics QuestaSim
 - Synopsys VCS

Other EDA vendors also provide simulators:

 - Aldec Riviera-PRO
 - Xilinx Vivado

Free and open source tools also exist, that support different subsets of the LRM:

 - Verilator

## Hello world
    // File 'test.sv'

    // Top module that gets instantiated automatically when simulation is started
    module test;
    
      // Thread gets started at the beginning of the simulation
      initial begin

        // Call to system task to print output in simulator console
        $display("Hello world!");
      end
    
    endmodule

Running in Cadence Incisive:
    
    irun test.sv

