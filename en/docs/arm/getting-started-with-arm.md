---
title: "Getting started with arm"
slug: "getting-started-with-arm"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Variants
There are many different variants of the ARM architecture and implementations that have evolved over time.  The notation can be confusing.  For instance, *arm7* and <em>arm<b>v</b>7</em>, are completely different.  The first is a CPU implementation; the second is a CPU architecture.  The *architecture*, also called a *family*, is a set of machine instructions (or **ISA** for *instruction set architecture*) that are generally compatible.  See: [Wikipedia's list of ARM microarchitectures][3] for more.

**Related tags:** 

 - [tag:thumb] - the first version consisted of a reduced set of 16bit instructions.  **thumb2**, introduced with armv6, includes a mix of 16 and 32bit instructions, extended further in armv7 such that it can do most things the normal ARM ISA can.
 - [tag:neon] - a SIMD extension for ARM CPUs
 - [tag:cortex-m] - an embedded ISA of the armv7 that only supports **thumb2**.
 - [tag:cortex-a] - the application version of armv7 ISA.
 - [tag:arm64] - the eighth arm architecture (armv8) includes 64bit registers.
 - [tag:trust-zone] - a security feature in some armv6, armv8 and armv7 CPUs.
 - [tag:amba] - bus or interconnect specification used between CPUs and peripherals.

Other CPU specific tags exist, such as [tag:cortex-m3].  Often those posts apply to other *cortex-m* CPUs and/or the difference between the versions is important to understand.  Also newer future CPUs may extend a specific CPU and questions in that tag maybe relevant.

If you are only posting to the tag [tag:arm] try to give some specifics about the system you are using.

  [1]: http://en.wikipedia.org/wiki/ARM_architecture
  [2]: http://infocenter.arm.com/help/index.jsp
  [3]: http://en.wikipedia.org/wiki/List_of_ARM_microarchitectures

## Build and Run ARM Assembly
To run ARM assembly code you will need a machine with an ARM processor.

If you are on Linux you can use the following commands to compile your program:

`as -o prog_object.o my_prog_source.s`

Link to get the executable:

`ld -o run_prog prog_object.o`

Run using:

`./run_prog`

If you run `echo $?` it will return the value stored in R0

