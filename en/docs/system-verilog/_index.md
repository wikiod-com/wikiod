---
title : system-verilog Tutorial
slug : system-verilog-tutorial
weight : 9953
draft : false
images : []
type : docs
---

SystemVerilog is the successor language to [Verilog](https://www.wikiod.com/verilog). Originally created by Accellera as an extension language to Verilog [IEEE Std 1364-2001](https://standards.ieee.org/findstds/standard/1364-2001.html), SystemVerilog was accepted as an IEEE standard in 2005. In 2009, IEEE merged Verilog (IEEE 1364) into SystemVerilog (IEEE 1800) as a unified language. Like its predecessor, SystemVerilog is supported by many FPGA (Field Programmable Gate Array) vendors and ASIC (Application Specific Integrated Circuit) tool vendors. SystemVerilog was created to enhance HDL design development and has dedicated features for verification. 

SystemVerilog consists of 3 main sub-languages:

* Design directives : Allows designers to write RTL more concise, explicit, and flags mistakes traditionally not found until synthesis.

* Object oriented classes : Used for verification, allows test-bench code to be more flexible and reusable. This capability spurred the creation of verification methodologies: [OVM](http://www.ovmworld.org), [VMM](https://www.vmmcentral.org), [UVM](http://accellera.org/downloads/standards/uvm)

* Assertions : Used for verification and coverage of protocols and internal sequential signals. 

