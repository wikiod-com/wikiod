---
title: "Tuples"
slug: "tuples"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

The most trivial data-structure, after a singular value, is the tuple.

## Syntax
 - (A, B, C) // a three-tuple (a tuple with three elements), whose first element has type A, second type B, and third type C
 - (A, B) // a two-tuple, whose two elements have type A and B respectively
 - (A,) // a one-tuple (note the trailing `,`), which holds only a single element of type A
 - () // the empty tuple, which is both a type, and that type's only element

## Looking inside tuples
To access elements of a tuple directly, you can use the format `.n` to access the `n`-th element

    let x = ("hello", 42, true);
    assert_eq!(x.0, "hello");
    assert_eq!(x.1, 42);
    assert_eq!(x.2, true);

You can also partially move out of a tuple

    let x = (String::from("hello"), 42);
    let (s, _) = x;
    let (_, n) = x;
    println!("{}, {}", s, n);
    // the following would fail however, since x.0 has already been moved
    // let foo = x.0;

## Matching tuple values
Rust programs use pattern matching extensively to deconstruct values, whether using `match`, `if let`, or deconstructing `let` patterns. Tuples can be deconstructed as you might expect using `match`

    fn foo(x: (&str, isize, bool)) {
        match x {
            (_, 42, _) => println!("it's 42"),
            (_, _, false) => println!("it's not true"),
            _ => println!("it's something else"),
        }
    }

or with `if let`

    fn foo(x: (&str, isize, bool)) {
        if let (_, 42, _) = x {
            println!("it's 42");
        } else {
            println!("it's something else");
        }
    }

you can also bind inside the tuple using `let`-deconstruction

    fn foo(x: (&str, isize, bool)) {
        let (_, n, _) = x;
        println!("the number is {}", n);
    }

## Basics
A tuple is simply a concatenation of multiple values:

 - of possibly different types
 - whose number and types is known statically

For example, `(1, "Hello")` is a 2 elements tuple composed of a `i32` and a `&str`, and its type is denoted as `(i32, &'static str)` in a similar fashion as its value.

To access an element of a tuple, one just simply uses its index:

    let tuple =  (1, "Hello");
    println!("First element: {}, second element: {}", tuple.0, tuple.1);

Because the tuple is built-in, it is also possible to use [pattern matching][1] on tuples:

    match (1, "Hello") {
        (i, _) if i < 0 => println!("Negative integer: {}", i),
        (_, s) => println!("{} World", s),
    }

---

**Special Cases**

The 0 element tuple: `()` is also called the *unit*, *unit type* or *singleton type* and is used to denote the absence of meaningful values. It is the default return type of functions (when `->` is not specified). *See also: [What type is the "type ()" in Rust?](http://stackoverflow.com/questions/38663359/what-type-is-the-type-in-rust)*.

The 1 element tuple: `(a,)`, with the trailing comma, denotes a 1 element tuple. The form without a comma `(a)` is interpreted as an expression enclosed in parentheses, and evaluates to just `a`.

And while we are at it, trailing commas are always accepted: `(1, "Hello",)`.

---

**Limitations**

The Rust language today does not support *variadics*, besides tuples. Therefore, it is not possible to simply implement a trait for all tuples and as a result the standard traits are only implemented for tuples up to a limited number of elements (today, up to 12 included). Tuples with more elements are supported, but do not implement the standard traits (though you can implement your own traits for them).

This restriction will hopefully be lifted in the future.


  [1]: https://www.wikiod.com/rust/pattern-matching

## Tuple types and tuple values
Rust tuples, as in most other languages, are fixed-size lists whose elements can all be of different types.

    // Tuples in Rust are comma-separated values or types enclosed in parentheses.
    let _ = ("hello", 42, true);
    // The type of a tuple value is a type tuple with the same number of elements.
    // Each element in the type tuple is the type of the corresponding value element.
    let _: (i32, bool) = (42, true);
    // Tuples can have any number of elements, including one ..
    let _: (bool,) = (true,);
    // .. or even zero!
    let _: () = ();
    // this last type has only one possible value, the empty tuple ()
    // this is also the type (and value) of the return value of functions
    // that do not return a value ..
    let _: () = println!("hello");
    // .. or of expressions with no value.
    let mut a = 0;
    let _: () = if true { a += 1; };

## Unpacking Tuples
    // It's possible to unpack tuples to assign their inner values to variables
    let tup = (0, 1, 2);
    // Unpack the tuple into variables a, b, and c
    let (a, b, c) = tup;
    
    assert_eq!(a, 0);
    assert_eq!(b, 1);

    // This works for nested data structures and other complex data types
    let complex = ((1, 2), 3, Some(0));
    
    let (a, b, c) = complex;
    let (aa, ab) = a;

    assert_eq!(aa, 1);
    assert_eq!(ab, 2);

