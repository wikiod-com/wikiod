---
title: "Signal handling"
slug: "signal-handling"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

Rust doesn't have a proper and idiomatic and safe way to lydiate with OS signals but there are some crates that provide signal handling but they are highly *experimental* and *unsafe* so be careful when using them.

However there is a discussion in the [rust-lang/rfcs][1] repository about implementing native signal handling for rust.

RFCs discussion: https://github.com/rust-lang/rfcs/issues/1368


  [1]: https://github.com/rust-lang/rfcs "RFCs"

## Signal handling with chan-signal crate
The [chan-signal][1] crate provides a solution to handle OS signal using channels, altough this crate is **experimental** and should be used **carefully**.

Example taken from [BurntSushi/chan-signal][2].

    #[macro_use]
    extern crate chan;
    extern crate chan_signal;
    
    use chan_signal::Signal;
    
    fn main() {
        // Signal gets a value when the OS sent a INT or TERM signal.
        let signal = chan_signal::notify(&[Signal::INT, Signal::TERM]);
        // When our work is complete, send a sentinel value on `sdone`.
        let (sdone, rdone) = chan::sync(0);
        // Run work.
        ::std::thread::spawn(move || run(sdone));
    
        // Wait for a signal or for work to be done.
        chan_select! {
            signal.recv() -> signal => {
                println!("received signal: {:?}", signal)
            },
            rdone.recv() => {
                println!("Program completed normally.");
            }
        }
    }
    
    fn run(_sdone: chan::Sender<()>) {
        println!("Running work for 5 seconds.");
        println!("Can you send a signal quickly enough?");
        // Do some work.
        ::std::thread::sleep_ms(5000);
    
        // _sdone gets dropped which closes the channel and causes `rdone`
        // to unblock.
    }


  [1]: https://crates.io/crates/chan-signal "crates.io"
  [2]: https://github.com/BurntSushi/chan-signal "GitHub"

## Handling signals with nix crate.
The [nix][1] crate provides an UNIX Rust API to handle signals, however it requires using **unsafe** rust so you should be **careful**.

    use nix::sys::signal;
    
    extern fn handle_sigint(_:i32) {
        // Be careful here...
    }
    
    fn main() {
        let sig_action = signal::SigAction::new(handle_sigint,
                                              signal::SockFlag::empty(),
                                              signal::SigSet::empty());
        signal::sigaction(signal::SIGINT, &sig_action);
    }


  [1]: https://crates.io/crates/nix "crates.io"

## Tokio Example
The [tokio-signal](https://github.com/alexcrichton/tokio-signal) crate provides a tokio-based solution for handling signals. It's still in it's early stages though.

```rust
extern crate futures;
extern crate tokio_core;
extern crate tokio_signal;

use futures::{Future, Stream};
use tokio_core::reactor::Core
use tokio_signal::unix::{self as unix_signal, Signal};
use std::thread::{self, sleep};
use std::time::Duration;
use std::sync::mpsc::{channel, Receiver};

fn run(signals: Receiver<i32>) {
    loop {
        if let Some(signal) = signals.try_recv() {
            eprintln!("received {} signal");
        }
        sleep(Duration::from_millis(1));
    }
}

fn main() {
    // Create channels for sending and receiving signals
    let (signals_tx, signals_rx) = channel();

    // Execute the program with the receiving end of the channel
    // for listening to any signals that are sent to it.
    thread::spawn(move || run(signals_rx));

    // Create a stream that will select over SIGINT, SIGTERM, and SIGHUP signals.
    let signals = Signal::new(unix_signal::SIGINT, &handle).flatten_stream()
        .select(Signal::new(unix_signal::SIGTERM, &handle).flatten_stream())
        .select(Signal::new(unix_signal::SIGHUP, &handle).flatten_stream());

    // Execute the event loop that will listen for and transmit received
    // signals to the shell.
    core.run(signals.for_each(|signal| {
        let _ = signals_tx.send(signal);
        Ok(())
    })).unwrap();
}
```

