---
title: "Futures and Async IO"
slug: "futures-and-async-io"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

[futures-rs](https://github.com/alexcrichton/futures-rs) is a library that implements zero-cost futures and streams in Rust.

The core concepts of the [futures](https://github.com/alexcrichton/futures-rs) crate are [Future](https://docs.rs/futures/0.1/futures/future/trait.Future.html) and [Stream](https://docs.rs/futures/0.1/futures/stream/trait.Stream.html).

## Creating a future with oneshot function
There are some general [`Future`](https://docs.rs/futures/0.1/futures/future/trait.Future.html) trait implementations in the [futures](https://github.com/alexcrichton/futures-rs) crate. One of them is implemented in [`futures::sync::oneshot`](https://docs.rs/futures/0.1/futures/sync/oneshot/index.html) module and is available through `futures::oneshot` function:

    extern crate futures;
    
    use std::thread;
    use futures::Future;
    
    fn expensive_computation() -> u32 {
        // ...
        200
    }
    
    fn main() {
        // The oneshot function returns a tuple of a Sender and a Receiver.
        let (tx, rx) = futures::oneshot();
    
        thread::spawn(move || {
            // The complete method resolves a values.
            tx.complete(expensive_computation());
        });
    
        // The map method applies a function to a value, when it is resolved.
        let rx = rx.map(|x| {
            println!("{}", x);
        });
    
        // The wait method blocks current thread until the value is resolved.
        rx.wait().unwrap();
    }

