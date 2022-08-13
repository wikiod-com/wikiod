---
title: "Rust Style Guide"
slug: "rust-style-guide"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

Although there is no official Rust style guide, the following examples show the conventions adopted by most Rust projects. Following these conventions will align your project's style with that of the standard library, making it easier for people to see the logic in your code.

The official Rust style guidelines were available in the [`rust-lang/rust`](https://github.com/rust-lang/rust/tree/528c6f3ed6a23a374dc5a40582d1bea2f2cfda65/src/doc/style/style) repository on GitHub, but they have recently been removed, pending migration to the [`rust-lang-nursery/fmt-rfcs`](https://github.com/rust-lang-nursery/fmt-rfcs) repository. Until new guidelines are published there, you should try to follow the guidelines in the `rust-lang` repository.

You can use [rustfmt](https://github.com/rust-lang-nursery/rustfmt) and [clippy](https://github.com/Manishearth/rust-clippy) to automatically review your code for style issues and format it correctly. These tools can be installed using Cargo, like so:

    cargo install clippy
    cargo install rustfmt

To run them, you use:

    cargo clippy
    cargo fmt

## Naming
**Structs**

    // Structs use UpperCamelCase.
    pub struct Snafucator {
        
    }

    mod snafucators {
        // Try to avoid 'stuttering' by repeating 
        // the module name in the struct name.

        // Bad:
        pub struct OrderedSnafucator {
        
        }
          
        // Good:
        pub struct Ordered {
            
        }
    }

**Traits**

    // Traits use the same naming principles as 
    // structs (UpperCamelCase).
    trait Read {
        fn read_to_snafucator(&self) -> Result<(), Error>;
    }

**Crates and Modules**

    // Modules and crates should both use snake_case.
    // Crates should try to use single words if possible.
    extern crate foo;
    mod bar_baz {
        mod quux {

        }
    }

**Static Variables and Constants**

    // Statics and constants use SCREAMING_SNAKE_CASE.
    const NAME: &'static str = "SCREAMING_SNAKE_CASE";

**Enums**

    // Enum types and their variants **both** use UpperCamelCase.
    pub enum Option<T> {
       Some(T),
       None
    }

**Functions and Methods**

    // Functions and methods use snake_case
    fn snake_cased_function() {

    }

**Variable bindings**

    // Regular variables also use snake_case
    let foo_bar = "snafu";

**Lifetimes**

    // Lifetimes should consist of a single lower case letter. By 
    // convention, you should start at 'a, then 'b, etc.

    // Good:
    struct Foobar<'a> {
        x: &'a str
    }

    // Bad:
    struct Bazquux<'stringlife> {
        my_str: &'stringlife str
    }

**Acronyms**

Variable names that contain acronyms, such as `TCP` should be styled as follows:

- For `UpperCamelCase` names, the **first letter** should be capitalised (e.g. `TcpClient`)
- For `snake_case` names, there should be no capitalisation (e.g. `tcp_client`)
- For `SCREAMING_SNAKE_CASE` names, the acronym should be completely capitalised (e.g. `TCP_CLIENT`)

## Whitespace
**Line Length**

    // Lines should never exceed 100 characters. 
    // Instead, just wrap on to the next line.
    let bad_example = "So this sort of code should never really happen, because it's really hard to fit on the screen!";

**Indentation**

    // You should always use 4 spaces for indentation. 
    // Tabs are discouraged - if you can, set your editor to convert
    // a tab into 4 spaces.
    let x = vec![1, 3, 5, 6, 7, 9];
    for item in x {
        if x / 2 == 3 {
            println!("{}", x);
        }
    }

**Trailing Whitespace**

Trailing whitespace at the end of files or lines should be deleted. 

**Binary Operators**

    // For clarity, always add a space when using binary operators, e.g.
    // +, -, =, *
    let bad=3+4;
    let good = 3 + 4;

This also applies in attributes, for example:

    // Good:
    #[deprecated = "Don't use my class - use Bar instead!"]

    // Bad:
    #[deprecated="This is broken"]

**Semicolons**

    // There is no space between the end of a statement
    // and a semicolon.

    let bad = Some("don't do this!") ;
    let good: Option<&str> = None;

**Aligning Struct Fields**

    // Struct fields should **not** be aligned using spaces, like this:
    pub struct Wrong {
        pub x  : i32,
        pub foo: i64 
    }

    // Instead, just leave 1 space after the colon and write the type, like this:
    pub struct Right {
        pub x: i32,
        pub foo: i64
    }

**Function Signatures**

    // Long function signatures should be wrapped and aligned so that
    // the starting parameter of each line is aligned
    fn foo(example_item: Bar, another_long_example: Baz, 
           yet_another_parameter: Quux) 
           -> ReallyLongReturnItem {
        // Be careful to indent the inside block correctly!
    }

**Braces**

    // The starting brace should always be on the same line as its parent.
    // The ending brace should be on its own line.
    fn bad()
    {
        println!("This is incorrect.");
    }

    struct Good {
        example: i32
    }

    struct AlsoBad {
        example: i32 }


## Imports
You should order your imports and declarations like so:

- `extern crate` declarations
- `use` imports
   - External imports from other crates should come first
- Re-exports (`pub use`)

## Creating Crates
**Preludes and Re-Exports**

    // To reduce the amount of imports that users need, you should
    // re-export important structs and traits.
    pub use foo::Client;
    pub use bar::Server;

Sometimes, crates use a `prelude` module to contain important structs, just like `std::io::prelude`. Usually, these are imported with `use std::io::prelude::*;`



## Types
**Type Annotations**

    // There should be one space after the colon of the type
    // annotation. This rule applies in variable declarations,
    // struct fields, functions and methods.

    // GOOD:
    let mut buffer: String = String::new();
    // BAD:
    let mut buffer:String = String::new();
    let mut buffer : String = String::new();

**References**

    // The ampersand (&) of a reference should be 'touching'
    // the type it refers to.

    // GOOD:
    let x: &str = "Hello, world.";
    // BAD:
    fn fooify(x: & str) {
        println!("{}", x);
    }

---
    // Mutable references should be formatted like so:
    fn bar(buf: &mut String) {
    
    }



