---
title: "Unsafe Guidelines"
slug: "unsafe-guidelines"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Explain why certain things are marked `unsafe` in Rust, and why we might need to use this escape hatch in certain (rare) situations.

## Data Races
Data races occur when a piece of memory is updated by one party while another tries to read or update it simultaneously (without synchronization between the two). Let's look at the classic example of a data race using a shared counter.

    use std::cell::UnsafeCell;
    use std::sync::Arc;
    use std::thread;

    // `UnsafeCell` is a zero-cost wrapper which informs the compiler that "what it
    // contains might be shared mutably." This is used only for static analysis, and
    // gets optimized away in release builds.
    struct RacyUsize(UnsafeCell<usize>);

    // Since UnsafeCell is not thread-safe, the compiler will not auto-impl Sync for
    // any type containig it. And manually impl-ing Sync is "unsafe".
    unsafe impl Sync for RacyUsize {}

    impl RacyUsize {
        fn new(v: usize) -> RacyUsize {
            RacyUsize(UnsafeCell::new(v))
        }

        fn get(&self) -> usize {
            // UnsafeCell::get() returns a raw pointer to the value it contains
            // Dereferencing a raw pointer is also "unsafe"
            unsafe { *self.0.get() }
        }

        fn set(&self, v: usize) { // note: `&self` and not `&mut self`
            unsafe { *self.0.get() = v }
        }
    }

    fn main() {
        let racy_num = Arc::new(RacyUsize::new(0));

        let mut handlers = vec![];
        for _ in 0..10 {
            let racy_num = racy_num.clone();
            handlers.push(thread::spawn(move || {
                for i in 0..1000 {
                    if i % 200 == 0 {
                        // give up the time slice to scheduler
                        thread::yield_now();
                        // this is needed to interleave the threads so as to observe
                        // data race, otherwise the threads will most likely be
                        // scheduled one after another.
                    }

                    // increment by one
                    racy_num.set(racy_num.get() + 1);
                }
            }));
        }

        for th in handlers {
            th.join().unwrap();
        }

        println!("{}", racy_num.get());
    }

The output will almost always be less than `10000` (10 threads × 1000) when run on a multi-core processor.

In this example, a data race has produced a logically wrong but still meaningful value. This is because only a single [word][1] was involved in the race and thus an update couldn't have partially changed it. But data races in general can produce corrupt values that are invalid for a type (type unsafe) when the object being raced for spans multiple words, and/or produce values that point to invalid memory locations (memory unsafe) when pointers are involved.

However, careful usage of atomic primitives can enable construction of very efficient data structures which may internally need to do some of these "unsafe" operations to perform actions that are not statically verifiable by Rust's type system, but correct overall (i.e. to build a safe abstraction).


  [1]: https://en.wikipedia.org/wiki/Word_(computer_architecture)

