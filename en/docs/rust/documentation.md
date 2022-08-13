---
title: "Documentation"
slug: "documentation"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Rust's compiler has several handy features to make documenting your project quick and easy.  You can use compiler lints to enforce documentation for each function, and have tests built in to your examples.

## Syntax
- /// Outer documentation comment (applies to the item below)

- //! Inner documentation comment (applies to the enclosing item)

- cargo doc # Generates documentation for this library crate.

- cargo doc --open # Generates documentation for this library crate and open browser.

- cargo doc -p CRATE # Generates documentation for the specified crate only.

- cargo doc --no-deps # Generates documentation for this library and no dependencies.

- cargo test # Runs unit tests and documentation tests.

[This](https://doc.rust-lang.org/stable/book/documentation.html) section of the 'Rust Book' may contain useful information about documentation and documentation tests.

Documentation comments can be applied to:

- Modules
- Structs
- Enums
- Methods
- Functions
- Traits and Trait Methods


## Documentation Lints
To ensure that all possible items are documented, you can use the `missing_docs` link to receive warnings/errors from the compiler. To receive warnings library-wide, place this attribute in your `lib.rs` file:

    #![warn(missing_docs)]

You can also receive errors for missing documentation with this lint:

    #![deny(missing_docs)]

By default, `missing_docs` are allowed, but you can explicitly allow them with this attribute:

    #![allow(missing_docs)]

This may be useful to place in one module to allow missing documentation for one module, but deny it in all other files.

## Documentation Tests
Code in documentation comments will automatically be executed  by `cargo test`. These are known as "documentation tests", and help to ensure that your examples work and will not mislead users of your crate.

You can import relative from the crate root (as if there were a hidden `extern crate mycrate;` at the top of the example)

    /// ```
    /// use mycrate::foo::Bar;
    /// ```

If your code may not execute properly in a documentation test, you can use the `no_run` attribute, like so:

    /// ```no_run
    /// use mycrate::NetworkClient;
    /// NetworkClient::login("foo", "bar");
    /// ```

You can also indicate that your code *should* panic, like this:

    /// ```should_panic
    /// unreachable!();
    /// ```


## Documentation Comments
Rust provides two types of documentation comments: inner documentation comments and outer documentation comments. Examples of each are provided below.

**Inner Documentation Comments**

    mod foo {
        //! Inner documentation comments go *inside* an item (e.g. a module or a 
        //! struct). They use the comment syntax //! and must go at the top of the
        //! enclosing item. 
        struct Bar {
            pub baz: i64
            //! This is invalid. Inner comments must go at the top of the struct,
            //! and must not be placed after fields.
        }
    }

**Outer Documentation Comments**

    /// Outer documentation comments go *outside* the item that they refer to.
    /// They use the syntax /// to distinguish them from inner comments.
    pub enum Test {
        Success,
        Fail(Error)
    }



## Conventions
    /// In documentation comments, you may use **Markdown**.
    /// This includes `backticks` for code, *italics* and **bold**.
    /// You can add headers in your documentation, like this:
    /// # Notes
    /// `Foo` is unsuitable for snafucating. Use `Bar` instead.
    struct Foo {
        ...
    }
---
    /// It is considered good practice to have examples in your documentation
    /// under an "Examples" header, like this:
    /// # Examples
    /// Code can be added in "fences" of 3 backticks.
    ///
    /// ```
    /// let bar = Bar::new();
    /// ```
    ///
    /// Examples also function as tests to ensure the examples actually compile.
    /// The compiler will automatically generate a main() function and run the
    /// example code as a test when cargo test is run.
    struct Bar {
        ...
    }



