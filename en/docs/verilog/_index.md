---
title : verilog Tutorial
slug : verilog-tutorial
weight : 9950
draft : false
images : []
type : docs
---

Verilog is a hardware description language (HDL) that is used to design, simulate, and verify digital circuitry at a behavioral or register-transfer level. It is noteworthy for a reasons that distinguish it from "traditional" programming languages:

* There are two types of assignment, blocking and non-blocking, each with their own uses and semantics.
* Variables must be declared as either single-bit wide or with an explicit width.
* Designs are hierarchical, with the ability to instantiate modules that have a desired behaviour.
* In simulation (not typically in synthesis), `wire` variables may be in one of four states: 0, 1, floating (`z`), and undefined (`x`).

