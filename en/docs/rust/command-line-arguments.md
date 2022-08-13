---
title: "Command Line Arguments"
slug: "command-line-arguments"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Rust's standard library does not contain a proper argument parser (unlike `argparse` in Python), instead preferring to leave this to third-party crates. These examples will show the usage of both the standard library (to form a crude argument handler) and the `clap` library which can parse command-line arguments more effectively.

## Syntax
- use std::env; // Import the env module
- let args = env::args(); // Store an Args iterator in the args variable.

## Using clap
For larger command line programs, using `std::env::args()` is quite tedious and difficult to manage. You can use [`clap`](https://docs.rs/clap/2.13.0/clap/) to handle your command line interface, which will parse arguments, generate help displays and avoid bugs.

There are several *patterns* that you can use with `clap`, and each one provides a different amount of flexibility.

**Builder Pattern**

This is the most verbose (and flexible) method, so it is useful when you need fine-grained control of your CLI.

`clap` distinguishes between *subcommands* and *arguments*. Subcommands act like independent subprograms in your main program, just like `cargo run` and `git push`. They can have their own command-line options and inputs. Arguments are simple flags such as `--verbose`, and they can take inputs (e.g. `--message "Hello, world"`)


    extern crate clap;
    use clap::{Arg, App, SubCommand};
    
    fn main() {
        let app = App::new("Foo Server")
            .about("Serves foos to the world!")
            .version("v0.1.0")
            .author("Foo (@Example on GitHub)")
            .subcommand(SubCommand::with_name("run")
                .about("Runs the Foo Server")
                .arg(Arg::with_name("debug")
                    .short("D")
                    .about("Sends debug foos instead of normal foos.")))

        // This parses the command-line arguments for use.
        let matches = app.get_matches();

        // We can get the subcommand used with matches.subcommand(), which
        // returns a tuple of (&str, Option<ArgMatches>) where the &str
        // is the name of the subcommand, and the ArgMatches is an 
        // ArgMatches struct: 
        // https://docs.rs/clap/2.13.0/clap/struct.ArgMatches.html

        if let ("run", Some(run_matches)) = app.subcommand() {
            println!("Run was used!");
        }
    }

## Using std::env::args()
You can access the command line arguments passed to your program using the [`std::env::args()`](https://doc.rust-lang.org/std/env/fn.args.html) function. This returns an `Args` iterator which you can loop over or collect into a `Vec`.

**Iterating Through Arguments**

    use std::env;

    fn main() {
        for argument in env::args() {
            if argument == "--help" {
                println!("You passed --help as one of the arguments!");
            }
        }
    }

**Collecting into a `Vec`**

    use std::env;
    
    fn main() {
        let arguments: Vec<String> = env::args().collect();
        println!("{} arguments passed", arguments.len());
    }

You might get more arguments than you expect if you call your program like this:

    ./example

Although it looks like no arguments were passed, the first argument is (**usually**) the name of the executable. This isn't a guarantee though, so you should always validate and filter the arguments you get.

