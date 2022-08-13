---
title: "Random Number Generation"
slug: "random-number-generation"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Rust has a built in capability to provide random number generation through the *rand* crate. Once part of the Rust standard library, the functionality of the *rand* crate was separated to allow its development to stabilize separate to the rest of the Rust project. This topic will cover how to simply add the rand crate, then generate and output a random number in Rust.


There is built-in support for a RNG associated with each thread stored in thread-local storage. This RNG can be accessed via `thread_rng`, or used implicitly via `random`. This RNG is normally randomly seeded from an operating-system source of randomness, e.g. `/dev/urandom` on Unix systems, and will automatically reseed itself from this source after generating 32 KiB of random data.

An application that requires an entropy source for cryptographic purposes must use `OsRng`, which reads randomness from the source that the operating system provides (e.g. `/dev/urandom` on Unixes or `CryptGenRandom()` on Windows). The other random number generators provided by this module are not suitable for such purposes.

## Generating Two Random Numbers with Rand
Firstly, you'll need to add the crate into your Cargo.toml file as a dependency.

    [dependencies]
    rand = "0.3"

This will retrieve the `rand` crate from [crates.io][1]. Next, add this to your crate root.

    extern crate rand;

As this example is going to provide a simple output through the terminal, we'll create a main function and print two randomly generated numbers to the console. The thread local random number generator will be cached in this example. When generating multiple values, this can often prove more efficient.

    use rand::Rng;
    
    fn main() {
        
        let mut rng = rand::thread_rng();
        
        if rng.gen() { // random bool
            println!("i32: {}, u32: {}", rng.gen::<i32>(), rng.gen::<u32>())
        }
    }

When you run this example, you should see the following response in the console.

    $ cargo run
         Running `target/debug/so`
    i32: 1568599182, u32: 2222135793


  [1]: https://crates.io/crates/rand

## Generating Characters with Rand
To generate characters, you can utilize the thread-local random number generator function, `random`.
    
    fn main() {
        let tuple = rand::random::<(f64, char)>();
        println!("{:?}", tuple)
    }

For occasional or singular requests, such as the one above, this is a reasonable efficient method. However, if you intend to generate more than a handful of numbers, you will find caching the generator will be more efficient. 

You should expect to the see the following output in this case.

    $ cargo run
         Running `target/debug/so`
    (0.906881, '\u{9edc}')



