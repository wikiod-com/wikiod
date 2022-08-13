---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

Unlike many other languages, Rust has **two** main string types: `String` (a heap-allocated string type) and `&str` (a **borrowed** string, which does not use extra memory). Knowing the difference and when to use each is vital to understand how Rust works.

## Basic String manipulation
    fn main() {
        // Statically allocated string slice
        let hello = "Hello world";

        // This is equivalent to the previous one
        let hello_again: &'static str = "Hello world";

        // An empty String
        let mut string = String::new();

        // An empty String with a pre-allocated initial buffer
        let mut capacity = String::with_capacity(10);

        // Add a string slice to a String
        string.push_str("foo");

        // From a string slice to a String
        // Note: Prior to Rust 1.9.0 the to_owned method was faster
        // than to_string. Nowadays, they are equivalent.      
        let bar = "foo".to_owned();
        let qux = "foo".to_string();

        // The String::from method is another way to convert a
        // string slice to an owned String.
        let baz = String::from("foo");

        // Coerce a String into &str with &
        let baz: &str = &bar;
    }
***Note:*** Both the `String::new` and the `String::with_capacity` methods will create empty strings. However, the latter allocates an initial buffer, making it initially slower, but helping reduce subsequent allocations. If the final size of the String is known, `String::with_capacity` should be preferred.

## Split a string
    let strings = "bananas,apples,pear".split(",");

`split` returns an iterator.

    for s in strings {
      println!("{}", s)
    }

And can be "collected" in a `Vec` with the `Iterator::collect` method.

    let strings: Vec<&str> = "bananas,apples,pear".split(",").collect(); // ["bananas", "apples", "pear"]

## Breaking long string literals
Break regular string literals with the `\` character

    let a = "foobar";
    let b = "foo\
             bar";
    
    // `a` and `b` are equal.
    assert_eq!(a,b);

Break raw-string literals to separate strings, and join them with the `concat!` macro

    let c = r"foo\bar";
    let d = concat!(r"foo\", r"bar");
    
    // `c` and `d` are equal.
    assert_eq!(c, d);

## String slicing
    fn main() {
        let english = "Hello, World!";

        println!("{}", &english[0..5]); // Prints "Hello"
        println!("{}", &english[7..]);  // Prints "World!"
    }

Note that we need to use the `&` operator here. It takes a reference and thus gives the compiler information about the size of the slice type, which it needs to print it. Without the reference, the two `println!` calls would be a compile-time error.

Warning: Slicing works by **byte offset**, not character offset, and will panic when bounds are not on a character boundary:

    fn main() {
        let icelandic = "Halló, heimur!"; // note that “ó” is two-byte long in UTF-8

        println!("{}", &icelandic[0..6]); // Prints "Halló", “ó” lies on two bytes 5 and 6
        println!("{}", &icelandic[8..]);  // Prints "heimur!", the “h” is the 8th byte, but the 7th char
        println!("{}", &icelandic[0..5]); // Panics!
    }

This is also the reason why strings don't support simple indexing (eg. `icelandic[5]`).

## From borrowed to owned
    // all variables `s` have the type `String`
    let s = "hi".to_string();  // Generic way to convert into `String`. This works
                               // for all types that implement `Display`.
    
    let s = "hi".to_owned();   // Clearly states the intend of obtaining an owned object

    let s: String = "hi".into();       // Generic conversion, type annotation required
    let s: String = From::from("hi");  // in both cases!

    let s = String::from("hi");  // Calling the `from` impl explicitly -- the `From` 
                                 // trait has to be in scope!

    let s = format!("hi");       // Using the formatting functionality (this has some
                                 // overhead)

Apart from `format!()`, all of the methods above are equally fast.
    

