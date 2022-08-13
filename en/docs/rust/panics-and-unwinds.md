---
title: "Panics and Unwinds"
slug: "panics-and-unwinds"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

When Rust programs reach a state where a critical error has occurred, the `panic!` macro can be called to exit quickly (often compared, but subtly different, to an exception in other languages). Proper error handling should involve `Result` types, though this section will only discuss `panic!` and its concepts.

Panics don't always cause memory leaks or other resource leaks. In fact, panics typically preserve RAII invariants, running the destructors (Drop implementations) of structs as the stack unwinds. However, if there is a second panic during this process, the program simply aborts; at that point, RAII invariant guarantees are void.

## Try not to panic
In Rust, there are two main methods to indicate something has gone wrong in a program: A function returning a ([potentially custom-defined](https://www.wikiod.com/rust/error-handling#Custom Error Types)) `Err(E)`, from the [`Result<T, E>`](https://www.wikiod.com/rust/error-handling) type and a `panic!`.

Panicking is **not** an alternative for exceptions, which are commonly found in other languages. In Rust, a panic is to indicate something has gone seriously wrong and that it cannot continue. Here is an example from the `Vec` source for `push`:

    pub fn push(&mut self, value: T) {
        // This will panic or abort if we would allocate > isize::MAX bytes
        // or if the length increment would overflow for zero-sized types.
        if self.len == self.buf.cap() {
            self.buf.double();
        }
        ...
    }
If we run out of memory, there's not much else Rust can do, so it will either panic (the default behavior) or abort (which needs to be set with a compilation flag).

Panicking will unwind the stack, running destructors and ensuring that memory is cleaned up. Abort does not do this, and relies on the OS to clean it up properly.

Try to run the following program both normally, and with 

    [profile.dev]
    panic = "abort"
in your `Cargo.toml`.

    // main.rs
    struct Foo(i32);
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("Dropping {:?}!", self.0);
        }
    }
    fn main() {
        let foo = Foo(1);
        panic!("Aaaaaaahhhhh!");
    }





