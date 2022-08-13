---
title: "Parallelism"
slug: "parallelism"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

Parallelism is supported well by Rust's standard library through various classes such as the `std::thread` module, channels and atomics. This section will guide you through the usage of these types.

## Cross-thread communication with channels
Channels can be used to send data from one thread to another. Below is an example of a simple producer-consumer system, where the main thread produces the values 0, 1, ..., 9, and the spawned thread prints them:

    use std::thread;
    use std::sync::mpsc::channel;
    
    fn main() {
        // Create a channel with a sending end (tx) and a receiving end (rx).
        let (tx, rx) = channel();
    
        // Spawn a new thread, and move the receiving end into the thread.
        let join_handle = thread::spawn(move || {
            // Keep receiving in a loop, until tx is dropped!
            while let Ok(n) = rx.recv() { // Note: `recv()` always blocks
                println!("Received {}", n);
            }
        });
    
        // Note: using `rx` here would be a compile error, as it has been
        // moved into the spawned thread.
    
        // Send some values to the spawned thread. `unwrap()` crashes only if the
        // receiving end was dropped before it could be buffered.
        for i in 0..10 {
            tx.send(i).unwrap(); // Note: `send()` never blocks
        }
    
        // Drop `tx` so that `rx.recv()` returns an `Err(_)`.
        drop(tx);
    
        // Wait for the spawned thread to finish.
        join_handle.join().unwrap();
    }

## Starting a new thread
To start a new thread:

    use std::thread;
    
    fn main() {
        thread::spawn(move || {
            // The main thread will not wait for this thread to finish. That
            // might mean that the next println isn't even executed before the
            // program exits.
            println!("Hello from spawned thread");
        });

        let join_handle = thread::spawn(move || {
            println!("Hello from second spawned thread");
            // To ensure that the program waits for a thread to finish, we must
            // call `join()` on its join handle. It is even possible to send a
            // value to a different thread through the join handle, like the
            // integer 17 in this case:
            17
        });
    
        println!("Hello from the main thread");
        
        // The above three printlns can be observed in any order.

        // Block until the second spawned thread has finished.
        match join_handle.join() {
            Ok(x) => println!("Second spawned thread returned {}", x),
            Err(_) => println!("Second spawned thread panicked")
        }
    }

## Atomics and Memory Ordering
Atomic types are the building blocks of lock-free data structures and other concurrent types. A memory ordering, representing the strength of the memory barrier, should be specified when accessing/modifying an atomic type. Rust provides 5 memory ordering primitives: **Relaxed** (the weakest), **Acquire** (for reads a.k.a. loads), **Release** (for writes a.k.a. stores), **AcqRel** (equivalent to "Acquire-for-load and Release-for-store"; useful when both are involved in a single operation such as compare-and-swap), and **SeqCst** (the strongest). In the example below, we'll demonstrate how "Relaxed" ordering differs from "Acquire" and "Release" orderings.

    use std::cell::UnsafeCell;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Barrier};
    use std::thread;

    struct UsizePair {
        atom: AtomicUsize,
        norm: UnsafeCell<usize>,
    }

    // UnsafeCell is not thread-safe. So manually mark our UsizePair to be Sync.
    // (Effectively telling the compiler "I'll take care of it!")
    unsafe impl Sync for UsizePair {}

    static NTHREADS: usize = 8;
    static NITERS: usize = 1000000;

    fn main() {
        let upair = Arc::new(UsizePair::new(0));

        // Barrier is a counter-like synchronization structure (not to be confused
        // with a memory barrier). It blocks on a `wait` call until a fixed number
        // of `wait` calls are made from various threads (like waiting for all
        // players to get to the starting line before firing the starter pistol).
        let barrier = Arc::new(Barrier::new(NTHREADS + 1));

        let mut children = vec![];

        for _ in 0..NTHREADS {
            let upair = upair.clone();
            let barrier = barrier.clone();
            children.push(thread::spawn(move || {
                barrier.wait();

                let mut v = 0;
                while v < NITERS - 1 {
                    // Read both members `atom` and `norm`, and check whether `atom`
                    // contains a newer value than `norm`. See `UsizePair` impl for
                    // details.
                    let (atom, norm) = upair.get();
                    if atom > norm {
                        // If `Acquire`-`Release` ordering is used in `get` and
                        // `set`, then this statement will never be reached.
                        println!("Reordered! {} > {}", atom, norm);
                    }
                    v = atom;
                }
            }));
        }

        barrier.wait();

        for v in 1..NITERS {
            // Update both members `atom` and `norm` to value `v`. See the impl for
            // details.
            upair.set(v);
        }

        for child in children {
            let _ = child.join();
        }
    }

    impl UsizePair {
        pub fn new(v: usize) -> UsizePair {
            UsizePair {
                atom: AtomicUsize::new(v),
                norm: UnsafeCell::new(v),
            }
        }

        pub fn get(&self) -> (usize, usize) {
            let atom = self.atom.load(Ordering::Relaxed); //Ordering::Acquire

            // If the above load operation is performed with `Acquire` ordering,
            // then all writes before the corresponding `Release` store is
            // guaranteed to be visible below.

            let norm = unsafe { *self.norm.get() };
            (atom, norm)
        }

        pub fn set(&self, v: usize) {
            unsafe { *self.norm.get() = v };

            // If the below store operation is performed with `Release` ordering,
            // then the write to `norm` above is guaranteed to be visible to all
            // threads that "loads `atom` with `Acquire` ordering and sees the same
            // value that was stored below". However, no guarantees are provided as
            // to when other readers will witness the below store, and consequently
            // the above write. On the other hand, there is also no guarantee that
            // these two values will be in sync for readers. Even if another thread
            // sees the same value that was stored below, it may actually see a
            // "later" value in `norm` than what was written above. That is, there
            // is no restriction on visibility into the future.

            self.atom.store(v, Ordering::Relaxed); //Ordering::Release
        }
    }

Note: x86 architectures have strong memory model. [This post](http://preshing.com/20120930/weak-vs-strong-memory-models/) explains it in detail. Also take a look at [the Wikipedia page](https://en.wikipedia.org/wiki/Memory_ordering#Runtime_memory_ordering) for comparison of architectures.

## Cross-thread communication with Session Types
Session Types are a way to tell the compiler about the protocol you want to use to communicate between threads - not protocol as in HTTP or FTP, but the pattern of information flow between threads. This is useful since the compiler will now stop you from accidentally breaking your protocol and causing deadlocks or livelocks between threads - some of the most notoriously hard to debug issues, and a major source of Heisenbugs. Session Types work similarly to the channels described above, but can be more intimidating to start using. Here's a simple two-thread communication:

    // Session Types aren't part of the standard library, but are part of this crate.
    // You'll need to add session_types to your Cargo.toml file.
    extern crate session_types;

    // For now, it's easiest to just import everything from the library.
    use session_types::*;

    // First, we describe what our client thread will do. Note that there's no reason
    // you have to use a client/server model - it's just convenient for this example.
    // This type says that a client will first send a u32, then quit. `Eps` is
    // shorthand for "end communication".
    // Session Types use two generic parameters to describe the protocol - the first
    // for the current communication, and the second for what will happen next.
    type Client = Send<u32, Eps>;
    // Now, we define what the server will do: it will receive as u32, then quit.
    type Server = Recv<u32, Eps>;

    // This function is ordinary code to run the client. Notice that it takes    
    // ownership of a channel, just like other forms of interthread communication - 
    // but this one about the protocol we just defined.
    fn run_client(channel: Chan<(), Client>) {
        let channel = channel.send(42);
        println!("The client just sent the number 42!");
        channel.close();
    }

    // Now we define some code to run the server. It just accepts a value and prints
    // it.
    fn run_server(channel: Chan<(), Server>) {
        let (channel, data) = channel.recv();
        println!("The server received some data: {}", data);
        channel.close();
    }

    fn main() {
        // First, create the channels used for the two threads to talk to each other.
        let (server_channel, client_channel) = session_channel();

        // Start the server on a new thread
        let server_thread = std::thread::spawn(move || {
            run_server(server_channel);
        });

        // Run the client on this thread.
        run_client(client_channel);

        // Wait for the server to finish.
        server_thread.join().unwrap();
    }

You should notice that the main method looks very similar to the main method for cross-thread communication defined above, if the server were moved to its own function. If you were to run this, you would get the output:

    The client just sent the number 42!
    The server received some data: 42

in that order.

Why go through all the hassle of defining the client and server types? And why do we redefine the channel in the client and server? These questions have the same answer: the compiler will stop us from breaking the protocol! If the client tried to receive data instead of sending it (which would result in a deadlock in ordinary code), the program wouldn't compile, since the client's channel object *doesn't have a `recv` method on it.* Also, if we tried to define the protocol in a way that could lead to deadlock (for example, if both the client and server tried to receive a value), then compilation would fail when we create the channels. This is because `Send` and `Recv` are "Dual Types", meaning if the Server does one, the Client has to do the other - if both try to `Recv`, you're going to be in trouble. `Eps` is its own dual type, since it's fine for both the Client and Server to agree to close the channel.

Of course, when we do some operation on the channel, we move to a new state in the protocol, and the functions available to us might change - so we have to redefine the channel binding. Luckily, `session_types` takes care of that for us and always returns the new channel (except `close`, in which case there is no new channel). This also means that all of the methods on a channel take ownership of the channel as well - so if you forget to redefine the channel, the compiler will give you an error about that, too. If you drop a channel without closing it, that's a runtime error as well (unfortunately, that is impossible to check at compile time).

There are many more types of communication than just `Send` and `Recv` - for example, `Offer` gives the other side of the channel the ability to chose between two possible branches of the protocol, and `Rec` and `Var` work together to allow loops and recursion in the protocol. Many more examples of Session Types and other types are available in the `session_types` [GitHub repository](https://github.com/Munksgaard/session-types/tree/master/examples). The library's documentation can be found [here.](https://crates.fyi/crates/session_types/0.2.0/)

## Read-write locks
RwLocks allow a single producer provide any number of readers with data while preventing readers from seeing invalid or inconsistent data. 

The following example uses RwLock to show how a single producer thread can periodically increase a value while two consumers threads are reading the value. 

```
use std::time::Duration;
use std::thread;
use std::thread::sleep;
use std::sync::{Arc, RwLock };

fn main() {
    // Create an u32 with an inital value of 0
    let initial_value = 0u32;

    // Move the initial value into the read-write lock which is wrapped into an atomic reference
    // counter in order to allow safe sharing.
    let rw_lock = Arc::new(RwLock::new(initial_value));

    // Create a clone for each thread
    let producer_lock = rw_lock.clone();
    let consumer_id_lock = rw_lock.clone();
    let consumer_square_lock = rw_lock.clone();

    let producer_thread = thread::spawn(move || {
        loop {
            // write() blocks this thread until write-exclusive access can be acquired and retuns an 
            // RAII guard upon completion
            if let Ok(mut write_guard) = producer_lock.write() {
                // the returned write_guard implements `Deref` giving us easy access to the target value
                *write_guard += 1;

                println!("Updated value: {}", *write_guard);
            }

            // ^
            // |   when the RAII guard goes out of the scope, write access will be dropped, allowing
            // +~  other threads access the lock

            sleep(Duration::from_millis(1000));
        }
    });

    // A reader thread that prints the current value to the screen
    let consumer_id_thread = thread::spawn(move || {
        loop {
            // read() will only block when `producer_thread` is holding a write lock
            if let Ok(read_guard) = consumer_id_lock.read() {
                // the returned read_guard also implements `Deref`
                println!("Read value: {}", *read_guard);
            }

            sleep(Duration::from_millis(500));
        }
    });

    // A second reader thread is printing the squared value to the screen. Note that readers don't
    // block each other so `consumer_square_thread` can run simultaneously with `consumer_id_lock`.
    let consumer_square_thread = thread::spawn(move || {
        loop {
            if let Ok(lock) = consumer_square_lock.read() {
                let value = *lock;
                println!("Read value squared: {}", value * value);
            }

            sleep(Duration::from_millis(750));
        }
    });

    let _ = producer_thread.join();
    let _ = consumer_id_thread.join();
    let _ = consumer_square_thread.join();
}
```
Example output:
```
Updated value: 1
Read value: 1
Read value squared: 1
Read value: 1
Read value squared: 1
Updated value: 2
Read value: 2
Read value: 2
Read value squared: 4
Updated value: 3
Read value: 3
Read value squared: 9
Read value: 3
Updated value: 4
Read value: 4
Read value squared: 16
Read value: 4
Read value squared: 16
Updated value: 5
Read value: 5
Read value: 5
Read value squared: 25
...(Interrupted)...
```

