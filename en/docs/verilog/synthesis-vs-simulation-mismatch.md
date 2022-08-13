---
title: "Synthesis vs Simulation mismatch"
slug: "synthesis-vs-simulation-mismatch"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

A good explanation of this topic is in http://www.sunburst-design.com/papers/CummingsSNUG1999SJ_SynthMismatch.pdf


## Comparison
wire d = 1'bx; // say from previous block. Will be 1 or 0 in hardware

if (d == 1'b) // false in simulation. May be true of false in hardware


## Sensitivity list
    wire a;
    wire b;
    reg q;
        
    always @(a) // b missing from sensativity list
     q = a & b; // In simulation q will change only when a changes

In hardware, q will change whenever a or b changes.
  

