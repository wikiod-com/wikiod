---
title: "Auto-dereferencing"
slug: "auto-dereferencing"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## The dot operator
The `.` operator in Rust comes with a lot of magic! When you use `.`, the compiler will insert as many `*`s (dereferencing operations) necessary to find the method down the deref "tree". As this happens at compile time, there is no runtime cost of finding the method.

    let mut name: String = "hello world".to_string();
    // no deref happens here because push is defined in String itself
    name.push('!');

    let name_ref: &String = &name;
    // Auto deref happens here to get to the String. See below
    let name_len = name_ref.len();
    // You can think of this as syntactic sugar for the following line:
    let name_len2 = (*name_ref).len();

    // Because of how the deref rules work,
    // you can have an arbitrary number of references. 
    // The . operator is clever enough to know what to do.
    let name_len3 = (&&&&&&&&&&&&name).len();
    assert_eq!(name_len3, name_len);
    
Auto dereferencing also works for any type implementing [`std::ops::Deref`][1] trait. 

    let vec = vec![1, 2, 3];
    let iterator = vec.iter();

Here, `iter` is not a method of `Vec<T>`, but a method of `[T]`. It works because `Vec<T>` [implements `Deref`][2] with `Target=[T]` which lets `Vec<T>` turn into `[T]` when dereferenced by the `*` operator (which the compiler may insert during a `.`).


  [1]: https://doc.rust-lang.org/std/ops/trait.Deref.html
  [2]: https://doc.rust-lang.org/src/collections/vec.rs.html#1475-1485

## Simple Deref example


## Deref coercions
Given two types `T` and `U`, `&T` will coerce (implicitly convert) to `&U` if and only if `T` implements `Deref<Target=U>`  

This allows us to do things like this: 

    fn foo(a: &[i32]) {
        // code
    }
    
    fn bar(s: &str) {
        // code
    }
    
    let v = vec![1, 2, 3];
    foo(&v); // &Vec<i32> coerces into &[i32] because Vec<T> impls Deref<Target=[T]>

    let s = "Hello world".to_string();
    let rc = Rc::new(s);
    // This works because Rc<T> impls Deref<Target=T> ∴ &Rc<String> coerces into 
    // &String which coerces into &str. This happens as much as needed at compile time.
    bar(&rc); 
    
    
    

## Using Deref and AsRef for function arguments
For functions that need to take a collection of objects, slices are usually a good choice:

    fn work_on_bytes(slice: &[u8]) {}

Because `Vec<T>` and arrays `[T; N]` implement `Deref<Target=[T]>`, they can be easily coerced to a slice:

    let vec = Vec::new();
    work_on_bytes(&vec);

    let arr = [0; 10];
    work_on_bytes(&arr);

    let slice = &[1,2,3];
    work_on_bytes(slice); // Note lack of &, since it doesn't need coercing

However, instead of explicitly requiring a slice, the function can be made to accept any type that *can be* used as a slice:

    fn work_on_bytes<T: AsRef<[u8]>>(input: T) {
        let slice = input.as_ref();
    }

In this example the function `work_on_bytes` will take any type `T` that implements `as_ref()`, which returns a reference to `[u8]`.
    
    work_on_bytes(vec);
    work_on_bytes(arr);
    work_on_bytes(slice);
    work_on_bytes("strings work too!");



## Deref implementation for Option and wrapper structure


