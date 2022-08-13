---
title: "Inline Assembly"
slug: "inline-assembly"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- #![feature(asm)] // Enable the asm! macro feature gate
- asm!(\<template> : \<output> : \<input> : \<clobbers> : \<options>) // Emit the assembly template provided (e.g. "NOP", "ADD %eax, 4") with the given options.


## Inputs and outputs
    #![feature(asm)]
        
    #[cfg(any(target_arch="x86", target_arch="x86_64"))]
    fn subtract(first: i32, second: i32) {
       unsafe {
            // Output values must either be unassigned (let result;) or mutable.
            let result: i32;
            // Each value that you pass in will be in a certain register, which
            // can be accessed with $0, $1, $2...
            //
            // The registers are assigned from left to right, so $0 is the 
            // register containing 'result', $1 is the register containing 
            // 'first' and $2 is the register containing 'second'.
            //
            // Rust uses AT&T syntax by default, so the format is:
            // SUB source, destination
            // which is equivalent to:
            // destination -= source;
            //
            // Because we want to subtract the first from the second, 
            // we use the 0 constraint on 'first' to use the same
            // register as the output.
            // Therefore, we're doing:
            // SUB second, first
            // and getting the value of 'first'
            
            asm!("SUB $2, $0" : "=r"(result) : "0"(first), "r"(second));
            println!("{}", result);
        }
    }


LLVM's constraint codes can be found [here](http://llvm.org/docs/LangRef.html#supported-constraint-code-list), but this may vary depending on the version of LLVM used by your `rustc` compiler.

## The asm! macro
Inline assembly will only be supported in nightly versions of Rust until it is [stabilized](https://github.com/rust-lang/rust/issues/29722). To enable usage of the `asm!` macro, use the following feature attribute at the top of the main file (a *feature gate*):

     #![feature(asm)]

Then use the `asm!` macro in any `unsafe` block:

    fn do_nothing() {
        unsafe {
            asm!("NOP");
        }

        // asm!("NOP"); 
        // That would be invalid here, because we are no longer in an 
        // unsafe block.
    }


## Conditionally compile inline assembly
Use conditional compilation to ensure that code only compiles for the intended instruction set (such as `x86`). Otherwise code could become invalid if the program is compiled for another architecture, such as ARM processors.

    #![feature(asm)]
    
    // Any valid x86 code is valid for x86_64 as well. Be careful
    // not to write x86_64 only code while including x86 in the 
    // compilation targets!
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    fn do_nothing() {
        unsafe {
            asm!("NOP");
        }
    }

    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64"))]
    fn do_nothing() {
        // This is an alternative implementation that doesn't use any asm!
        // calls. Therefore, it should be safe to use as a fallback.
    }

