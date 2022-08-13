---
title: "Getting started with Rust"
slug: "getting-started-with-rust"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Advanced usage of println!
[`println!`](https://doc.rust-lang.org/std/macro.println!.html) (and its sibling, [`print!`](https://doc.rust-lang.org/std/macro.print!.html)) provides a convenient mechanism for producing and printing text that contains dynamic data, similar to the `printf` family of functions found in many other languages. Its first argument is a *format string*, which dictates how the other arguments should be printed as text. The format string may contain placeholders (enclosed in `{}`) to specify that a substitution should occur:

    // No substitution -- the simplest kind of format string
    println!("Hello World");
    // Output: Hello World

    // The first {} is substituted with a textual representation of
    // the first argument following the format string. The second {}
    // is substituted with the second argument, and so on.
    println!("{} {} {}", "Hello", true, 42);
    // Output: Hello true 42

At this point, you may be asking: How did `println!` know to print the boolean value `true` as the string "true"? `{}` is really an instruction to the formatter that the value should be converted to text using the [`Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html) trait. This trait is implemented for most primitive Rust types (strings, numbers, booleans, etc.), and is meant for "user-facing output". Hence, the number 42 will be printed in decimal as 42, and not, say, in binary, which is how it is stored internally.

How do we print types, then, that do *not* implement `Display`, examples being Slices (`[i32]`), vectors (`Vec<i32>`), or options (`Option<&str>`)? There is no clear user-facing textual representation of these (i.e. one you could trivially insert into a sentence). To facilitate the printing of such values, Rust also has the [`Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html) trait, and the corresponding `{:?}` placeholder. From the documentation: "`Debug` should format the output in a programmer-facing, debugging context." Let's see some examples:

    println!("{:?}", vec!["a", "b", "c"]);
    // Output: ["a", "b", "c"]

    println!("{:?}", Some("fantastic"));
    // Output: Some("fantastic")

    println!("{:?}", "Hello");
    // Output: "Hello"
    // Notice the quotation marks around "Hello" that indicate
    // that a string was printed.

`Debug` also has a built-in pretty-print mechanism, which you can enable by using the `#` modifier after the colon:

    println!("{:#?}", vec![Some("Hello"), None, Some("World")]);
    // Output: [
    //    Some(
    //        "Hello"
    //    ),
    //    None,
    //    Some(
    //        "World"
    //    )
    // ]

The format strings allow you to express fairly [complex substitutions](https://doc.rust-lang.org/std/fmt/):

    // You can specify the position of arguments using numerical indexes.
    println!("{1} {0}", "World", "Hello");
    // Output: Hello World

    // You can use named arguments with format
    println!("{greeting} {who}!", greeting="Hello", who="World");
    // Output: Hello World

    // You can mix Debug and Display prints:
    println!("{greeting} {1:?}, {0}", "and welcome", Some(42), greeting="Hello");
    // Output: Hello Some(42), and welcome

`println!` and friends will also warn you if you are trying to do something that won't work, rather than crashing at runtime:

    // This does not compile, since we don't use the second argument.
    println!("{}", "Hello World", "ignored");

    // This does not compile, since we don't give the second argument.
    println!("{} {}", "Hello");

    // This does not compile, since Option type does not implement Display
    println!("{}", Some(42));

At their core, the Rust printing macros are simply wrappers around the [`format!`](https://doc.rust-lang.org/std/macro.format!.html) macro, which allows constructing a string by stitching together textual representations of different data values. Thus, for all the examples above, you can substitute `println!` for `format!` to store the formatted string instead of printing it:

    let x: String = format!("{} {}", "Hello", 42);
    assert_eq!(x, "Hello 42");

## Console output without macros
    // use Write trait that contains write() function
    use std::io::Write;
    
    fn main() {
        std::io::stdout().write(b"Hello, world!\n").unwrap();
    }

- The `std::io::Write` trait is designed for objects which accept byte streams. In this case, a handle to standard output is acquired with `std::io::stdout()`.

- `Write::write()` accepts a byte slice (`&[u8]`), which is created with a byte-string literal (`b"<string>"`). `Write::write()` returns a `Result<usize, IoError>`, which contains either the number of bytes written (on success) or an error value (on failure).

- The call to `Result::unwrap()` indicates that the call is expected to succeed (`Result<usize, IoError> -> usize`), and the value is discarded.

## Minimal example
To write the traditional Hello World program in Rust, create a text file called `hello.rs` containing the following source code:

    fn main() {
        println!("Hello World!");
    }

This defines a new function called `main`, which takes no parameters and returns no data. This is where your program starts execution when run. Inside it, you have a `println!`, which is a macro that prints text into the console.

To generate a binary application, invoke the Rust compiler by passing it the name of the source file:

```sh
$ rustc hello.rs
````

The resulting executable will have the same name as the main source module, so to run the program on a Linux or MacOS system, run:

````sh
$ ./hello
Hello World!
````

On a Windows system, run:

````sh
C:\Rust> hello.exe
Hello World!
````


## Getting started
# Installing

Before you can do anything using the Rust programming language, you’re going to need to acquire it—[either for Windows][1] or by using your terminal on *Unix-like* systems, where `$` symbolizes input into the terminal:

<!-- language: lang-bash -->

    $ curl https://sh.rustup.rs -sSf | sh

This will retrieve the required files and set up the latest version of Rust for you, no matter what system you’re on. For more information, see [project page][`rustup`].

_Note: Some Linux distributions (e.g. [Arch Linux]) provide `rustup` as a package, which can be installed instead. And although many *Unix-like* systems provide `rustc` and `cargo` as separate packages, it is still recommended to use [`rustup`] instead since it makes it much easier to manage multiple release channels and do cross-compilation._

[Arch Linux]: https://www.archlinux.org/packages/community/x86_64/rustup/
[`rustup`]: https://github.com/rust-lang-nursery/rustup.rs

# Rust Compiler

We can now check to see if *Rust* was in fact successfully installed onto our computers by running the following command either in our terminal—if on UNIX—or the command prompt—if on Windows:

<!-- language: lang-bash -->

    $ rustc --version

Should this command be successful, the version of *Rust*’s compiler installed onto our computers will be displayed before our eyes.

# Cargo

With Rust comes *Cargo*, which is a build tool used for managing your *Rust* packages and projects. To make sure this, too, is present on your computer, run the following inside the console—console referring to either terminal or command prompt depending on what system you’re on:

<!-- language: lang-bash -->

    $ cargo --version

Just like the equivalent command for the *Rust* compiler, this will return and display the current version of *Cargo*.

To create your first Cargo project, you may head to [Cargo][2].

Alternatively, you could compile programs directly using `rustc` as shown in [Minimal example][3].

  [1]: http://win.rustup.rs/
  [2]: https://www.wikiod.com/rust/cargo#Create new project
  [3]: https://www.wikiod.com/rust/getting-started-with-rust#Minimal example

