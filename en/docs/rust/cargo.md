---
title: "Cargo"
slug: "cargo"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

Cargo is Rust's package manager, used to manage *crates* (Rust's term for libraries/packages). Cargo predominantly fetches packages from [crates.io](http://crates.io/) and can manage complex dependency trees with specific version requirements (using semantic versioning). Cargo can also help build, run and manage Rust projects with `cargo build`, `cargo run` and `cargo test` (among other useful commands).

## Syntax
* cargo new crate_name [--bin]
* cargo init [--bin]
* cargo build [--release]
* cargo run [--release]
* cargo check
* cargo test
* cargo bench
* cargo update
* cargo package
* cargo publish
* cargo [un]install binary_crate_name
* cargo search crate_name
* cargo version
* cargo login api_key

 - At the moment, the `cargo bench` subcommand requires the nightly version of the compiler to operate effectively.

## Create new project
# Library

    cargo new my-library

This creates a new directory called `my-library` containing the cargo config file and a source directory containing a single Rust source file:

    my-library/Cargo.toml
    my-library/src/lib.rs

These two files will already contain the basic skeleton of a library, such that you can do a `cargo test` (from within `my-library` directory) right away to verify if everything works.

# Binary

    cargo new my-binary --bin

This creates a new directory called `my-binary` with a similar structure as a library:

    my-binary/Cargo.toml
    my-binary/src/main.rs

This time, `cargo` will have set up a simple Hello World binary which we can run right away with `cargo run`.

---

You can also create the new project in the current directory with the `init` sub-command:

    cargo init --bin

Just like above, remove the `--bin` flag to create a new library project. The name of the current folder is used as crate name automatically.

## Build project
# Debug

    cargo build

# Release

Building with the `--release` flag enables certain compiler optimizations that aren't done when building a debug build. This makes the code run faster, but makes the compile time a bit longer too. For optimal performance, this command should be used once a release build is ready.

    cargo build --release

## Running tests
# Basic Usage
    cargo test

# Show program output
    cargo test -- --nocapture

# Run specific example
    cargo test test_name

## Publishing a Crate
To publish a crate on [crates.io](https://crates.io), you must log in with Cargo (see '*Connecting Cargo to a Crates.io Account*').

You can package and publish your crate with the following commands:

    cargo package
    cargo publish

Any errors in your `Cargo.toml` file will be highlighted during this process. You should ensure that you **update your version** and ensure that your `.gitignore` or `Cargo.toml` file excludes any unwanted files.

## Connecting Cargo to a Crates.io Account
Accounts on crates.io are created by logging in with GitHub; you cannot sign up with any other method.

To connect your GitHub account to crates.io, click the '*Login with GitHub*' button in the top menu bar and authorise crates.io to access your account. This will then log you in to crates.io, assuming everything went well.

You must then find your **API key**, which can be found by clicking on your avatar, going to '*Account Settings*' and copying the line that looks like this:

    cargo login abcdefghijklmnopqrstuvwxyz1234567890rust

This should be pasted in your terminal/command line, and should authenticate you with your local `cargo` installation.

Be careful with your API key - it **must** be kept secret, like a password, otherwise your crates could be hijacked!


## Hello world program
This is a shell session showing how to create a "Hello world" program and run it with Cargo:

    $ cargo new hello --bin
    $ cd hello
    $ cargo run
       Compiling hello v0.1.0 (file:///home/rust/hello)
         Running `target/debug/hello`
    Hello, world!

After doing this, you can edit the program by opening `src/main.rs` in a text editor.

