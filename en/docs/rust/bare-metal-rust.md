---
title: "Bare Metal Rust"
slug: "bare-metal-rust"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

The Rust Standard Library ([`std`](https://doc.rust-lang.org/std/)) is compiled against only a handful of architectures. So, to compile to other architectures (that LLVM supports), Rust programs could opt not to use the entire `std`, and instead use just the portable subset of it, known as The Core Library ([`core`](https://doc.rust-lang.org/core/)).

## #![no_std] Hello, World!


    #![feature(start, libc, lang_items)]
    #![no_std]
    #![no_main]
    
    // The libc crate allows importing functions from C.
    extern crate libc;
    
    // A list of C functions that are being imported
    extern {
        pub fn printf(format: *const u8, ...) -> i32;
    }
    
    #[no_mangle]
    // The main function, with its input arguments ignored, and an exit status is returned
    pub extern fn main(_nargs: i32, _args: *const *const u8) -> i32 {
        // Print "Hello, World" to stdout using printf
        unsafe { 
            printf(b"Hello, World!\n" as *const u8);
        }
    
        // Exit with a return status of 0.
        0
    }
    
    #[lang = "eh_personality"] extern fn eh_personality() {}
    #[lang = "panic_fmt"] extern fn panic_fmt() -> ! { panic!() }


