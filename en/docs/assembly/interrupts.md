---
title: "Interrupts"
slug: "interrupts"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

**Why do we need Interrupts**

Lets imagine:
Our computer is connected to a keypad. We want to enter something. When we press the key nothing happens because the computer is dealing with different things and doesnt notice that we want something from him. We need Interrupts!

Interrupts are triggered by software (_INT_ 80h) or hardware (keypress), they behave like a _Call_ (they jump to a specific location, execute code and jump back again). 


## Working with Interrupts on the Z80:
The Z80 has no Interrupt table like modern processors. The Interrupts all execute the same code. In Interrupt Mode 1, they execute the code in a specific unchangeable location. In Interrupt Mode 2, they execute the code from the Pointer  register I points to. The Z80 has got a timer, that triggers the Interrupt all ~0.007s.

    EI      ;enables Interrupts
    DI      ;disables Interrupts
    IM 1    ;sets the Normal Interrupt Mode


    IM 2    ;sets the Advanced Interrupt Mode 
    LD I,$99;sets the Interrupt Pointer to $99 (just possible in IM 2)
    
    

