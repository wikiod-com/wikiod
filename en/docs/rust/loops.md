---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9810
type: docs
toc: true
---

## Syntax
* loop { *block* } // infinite loop
* while *condition* { *block* }
* while let *pattern* = *expr* { *block* }
* for *pattern* in *expr* { *block* } // expr must implement IntoIterator
* continue // jump to the end of the loop body, starting a new iteration if necessary
* break // stop the loop

* '*label*: loop { *block* }
* '*label*: while *condition* { *block* }
* '*label*: while let *pattern* = *expr* { *block* }
* '*label*: for *pattern* in *expr* { *block* }
* continue '*label* // jump to the end of the loop body labelled *label*, starting a new iteration if necessary
* break '*label* // stop the loop labelled *label*


## Basics
There are 4 looping constructs in Rust. All examples below produce the same output.

# Infinite Loops

    let mut x = 0;
    loop {
        if x > 3 { break; }
        println!("{}", x);
        x += 1;
    }

# While Loops

    let mut x = 0;
    while x <= 3 {
        println!("{}", x);
        x += 1;
    }

_Also see:_ [What is the difference between `loop` and `while true`?][1]

# Pattern-matched While Loops
These are sometimes known as `while let` loops for brevity.

    let mut x = Some(0);
    while let Some(v) = x {
        println!("{}", v);
        x = if v < 3 { Some(v + 1) }
            else     { None };
    }

This is equivalent to a `match` inside a `loop` block:

    let mut x = Some(0);
    loop {
        match x {
            Some(v) => {
                println!("{}", v);
                x = if v < 3 { Some(v + 1) }
                    else     { None };
            }
            _       => break,
        }
    }

# For Loops

In Rust, `for` loop can only be used with an "iterable" object (i.e. it should implement [`IntoIterator`][2]).

    for x in 0..4 {
        println!("{}", x);
    }

This is equivalent to the following snippet involving `while let`:

    let mut iter = (0..4).into_iter();
    while let Some(v) = iter.next() {
        println!("{}", v);
    }

_Note:_ `0..4` returns a [`Range` object][3] which already implements the [`Iterator` trait][4]. Therefore [`into_iter()`][2] is unnecessary, but is kept just to illustrate what `for` does. For an in-depth look, see the official docs on [`for` Loops and `IntoIterator`][5].

_Also see:_ https://www.wikiod.com/rust/iterators


  [1]: http://stackoverflow.com/questions/28892351/what-is-the-difference-between-loop-and-while-true
  [2]: https://doc.rust-lang.org/std/iter/trait.IntoIterator.html
  [3]: https://doc.rust-lang.org/std/ops/struct.Range.html
  [4]: https://doc.rust-lang.org/std/iter/trait.Iterator.html
  [5]: https://doc.rust-lang.org/1.10.0/std/iter/index.html#for-loops-and-intoiterator

## Loop Control
All looping constructs allow the use of `break` and `continue` statements. They affect the immediately surrounding (innermost) loop.

## Basic Loop Control

`break` terminates the loop:

    for x in 0..5 {
        if x > 2 { break; }
        println!("{}", x);
    }

#### Output

    0
    1
    2

---

`continue` finishes the current iteration early

    for x in 0..5 {
        if x < 2 { continue; }
        println!("{}", x);
    }

#### Output

    2
    3
    4

---

## Advanced Loop Control

Now, suppose we have nested loops and want to `break` out to the outer loop. Then, we can use loop labels to specify which loop a `break` or `continue` applies to. In the following example, `'outer` is the label given to the outer loop.
 
    'outer: for i in 0..4 {
        for j in i..i+2 {
            println!("{} {}", i, j);
            if i > 1 {
                continue 'outer;
            }
        }
        println!("--");
    }

#### Output

    0 0
    0 1
    --
    1 1
    1 2
    --
    2 2
    3 3

For `i > 1`, the inner loop was iterated only once and `--` was not printed.

---

_Note:_ Do not confuse a loop label with a lifetime variable. Lifetime variables only occurs beside an `&` or as a generic parameter within `<>`.

## More About For Loops
As mentioned in Basics, we can use anything which implements [`IntoIterator`][1] with the `for` loop:

    let vector = vec!["foo", "bar", "baz"]; // vectors implement IntoIterator
    for val in vector {
        println!("{}", val);
    }

Expected output:

    foo  
    bar  
    baz

Note that iterating over `vector` in this way consumes it (after the `for` loop, `vector` can not be used again). This is because `IntoIterator::into_iter` [moves][2] `self`.

`IntoIterator` is also implemented by `&Vec<T>` and `&mut Vec<T>` (yielding values with types `&T` and `&mut T` respectively) so you can prevent the move of `vector` by simply passing it by reference:

    let vector = vec!["foo", "bar", "baz"];
    for val in &vector {
        println!("{}", val);
    }
    println!("{:?}", vector);

Note that `val` is of type `&&str`, since `vector` is of type `Vec<&str>`.


  [1]: https://doc.rust-lang.org/std/iter/trait.IntoIterator.html
  [2]: https://doc.rust-lang.org/book/ownership.html#ownership

