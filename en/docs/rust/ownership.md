---
title: "Ownership"
slug: "ownership"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Ownership is one of the most important concepts in Rust, and it's something that isn't present in most other languages. The idea that a value can be *owned* by a particular variable is often quite difficult to understand, especially in languages where copying is implicit, but this section will review the different ideas surrounding ownership.

## Syntax
 - let x: &T = ... // x is an immutable reference
 - let x: &mut T = ... // x is an exclusive, mutable reference
 - let _ = &mut foo; // borrow foo mutably (i.e., exclusively)
 - let _ = &foo; // borrow foo immutably
 - let _ = foo; // move foo (requires ownership)

 - In much older versions of Rust (before 1.0; May 2015), an owned variable had a type starting with `~`. You may see this in very old examples.

## Ownership and borrowing
All values in Rust have exactly one owner. The owner is responsible for dropping that value when it goes out of scope, and is the only one who may *move* the ownership of the value. The owner of a value may give away *references* to it by letting other pieces of code *borrow* that value. At any given time, there may be any number of immutable references to a value:

    let owned = String::from("hello");
    // since we own the value, we may let other variables borrow it
    let immutable_borrow1 = &owned;
    // as all current borrows are immutable, we can allow many of them
    let immutable_borrow2 = &owned;
    // in fact, if we have an immutable reference, we are also free to
    // duplicate that reference, since we maintain the invariant that
    // there are only immutable references
    let immutable_borrow3 = &*immutable_borrow2;

**or** a single mutable reference to it (`ERROR` denotes a compile-time error):

    // for us to borrow a value mutably, it must be mutable
    let mut owned = String::from("hello");
    // we can borrow owned mutably
    let mutable_borrow = &mut owned;
    // but note that we cannot borrow owned *again*
    let mutable_borrow2 = &mut owned; // ERROR, already borrowed
    // nor can we cannot borrow owned immutably
    // since a mutable borrow is exclusive.
    let immutable_borrow = &owned; // ERROR, already borrowed

If there are outstanding references (either mutable or immutable) to a value, that value cannot be moved (i.e., its ownership given away). We would have to make sure all references were dropped first to be allowed to move a value:

    let foo = owned; // ERROR, outstanding references to owned
    let owned = String::from("hello");
    {
        let borrow = &owned;
        // ...
    } // the scope ends the borrow
    let foo = owned; // OK, owned and not borrowed

## Borrows and lifetimes
All values in Rust have a *lifetime*. A value's lifetime spans the segment of code from the value is introduced to where it is moved, or the end of the containing scope

    {
        let x = String::from("hello"); //             +
        // ...                                        :
        let y = String::from("hello"); //      +      |
        // ...                                 :      :
        foo(x) // x is moved                   |      = x's lifetime
        // ...                                 :
    } //                                       = y's lifetime

Whenever you borrow a value, the resulting reference has a *lifetime* that is tied to the lifetime of the value being borrowed:

    {
        let x = String::from("hello");
        let y = String::from("world");
        // when we borrow y here, the lifetime of the reference
        // stored in foo is equal to the lifetime of y
        // (i.e., between let y = above, to the end of the scope below)
        let foo = &y;
        // similarly, this reference to x is bound to the lifetime
        // of x --- bar cannot, for example, spawn a thread that uses
        // the reference beyond where x is moved below.
        bar(&x);
    }

## Ownership and function calls
Most of the questions around ownership come up when writing functions. When you specify the types of a function's arguments, you may choose *how* that value is passed in. If you only need read-only access, you can take an immutable reference:


    fn foo(x: &String) {
        // foo is only authorized to read x's contents, and to create
        // additional immutable references to it if it so desires.
        let y = *x; // ERROR, cannot move when not owned
        x.push_str("foo"); // ERROR, cannot mutate with immutable reference
        println!("{}", x.len()); // reading OK
        foo(x); // forwarding reference OK
    }

If `foo` needs to modify the argument, it should take an exclusive, mutable reference:

    fn foo(x: &mut String) {
        // foo is still not responsible for dropping x before returning,
        // nor is it allowed to. however, foo may modify the String.
        let x2 = *x; // ERROR, cannot move when not owned
        x.push_str("foo"); // mutating OK
        drop(*x); // ERROR, cannot drop value when not owned
        println!("{}", x.len()); // reading OK
    }

If you do not specify either `&` or `&mut`, you are saying that the function will take ownership of an argument. This means that `foo` is now also responsible for dropping `x`.

    fn foo(x: String) {
        // foo may do whatever it wishes with x, since no-one else has
        // access to it. once the function terminates, x will be dropped,
        // unless it is moved away when calling another function.
        let mut x2 = x; // moving OK
        x2.push_str("foo"); // mutating OK
        let _ = &mut x2; // mutable borrow OK
        let _ = &x2; // immutable borrow OK (note that &mut above is dropped)
        println!("{}", x2.len()); // reading OK
        drop(x2); // dropping OK
    }

## Ownership and the Copy trait
Some Rust types implement the [`Copy`](https://doc.rust-lang.org/std/marker/trait.Copy.html) trait. Types that are `Copy` can be moved without owning the value in question. This is because the contents of the value can simply be copied byte-for-byte in memory to produce a new, identical value. Most primitives in Rust (`bool`, `usize`, `f64`, etc.) are `Copy`.

    let x: isize = 42;
    let xr = &x;
    let y = *xr; // OK, because isize is Copy
    // both x and y are owned here

Notably, `Vec` and `String` are *not* `Copy`:

    let x = Vec::new();
    let xr = &x;
    let y = *xr; // ERROR, cannot move out of borrowed content

