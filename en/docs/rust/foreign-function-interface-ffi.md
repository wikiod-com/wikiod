---
title: "Foreign Function Interface (FFI)"
slug: "foreign-function-interface-ffi"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - #[link(name = "snappy")] // the foreign library to be linked to (optional)

   extern { ... } // list of function signatures in the foreign library

## Calling libc function from nightly rust


The `libc` crate is '[feature gated](https://www.rust-lang.org/en-US/faq.html#how-does-rust-language-versioning-work)' and can only be accessed on nightly Rust versions until it is considered stable.

    #![feature(libc)]
    extern crate libc;
    use libc::pid_t;
    
    #[link(name = "c")]
    extern {
        fn getpid() -> pid_t;
    }
    
    fn main() {
        let x = unsafe { getpid() };
        println!("Process PID is {}", x);
    }


