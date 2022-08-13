---
title: "Boxed values"
slug: "boxed-values"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Boxes are a very important part of Rust, and every rustacean should know what they are and how to use them

## Creating a Box
In stable Rust you create a Box by using the `Box::new` function.

    let boxed_int: Box<i32> = Box::new(1);

## Using Boxed Values
Because Boxes implement the `Deref<Target=T>`, you can use boxed values just like the value they contain.

    let boxed_vec = Box::new(vec![1, 2, 3]);
    println!("{}", boxed_vec.get(0));

If you want to pattern match on a boxed value, you may have to dereference the box manually.

    struct Point {
        x: i32,
        y: i32,
    }
    
    let boxed_point = Box::new(Point { x: 0, y: 0});
    // Notice the *. That dereferences the boxed value into just the value
    match *boxed_point {
        Point {x, y} => println!("Point is at ({}, {})", x, y),
    }



## Using Boxes to Create Recursive Enums and Structs
If you try and create a recursive enum in Rust without using Box, you will get a compile time error saying that the enum can't be sized.

    // This gives an error!
    enum List {
        Nil,
        Cons(i32, List)
    }

In order for the enum to have a defined size, the recursively contained value must be in a Box.

    // This works!
    enum List {
        Nil,
        Cons(i32, Box<List>)
    }

This works because Box<T> always has the same size no matter what T is, which allows Rust to give List a size.


