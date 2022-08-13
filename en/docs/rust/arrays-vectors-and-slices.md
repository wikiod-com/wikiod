---
title: "Arrays, Vectors and Slices"
slug: "arrays-vectors-and-slices"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Arrays
An array is a stack-allocated, statically-sized list of objects of a single type.

Arrays are usually created by enclosing a list of elements of a given type between square brackets. The type of an array is denoted with the special syntax: `[T; N]` where `T` is the type of its elements and `N` their count, both of which must be known at compilation time.

For example, `[4u64, 5, 6]` is a 3-element array of type `[u64; 3]`. Note: `5` and `6` are inferred to be of type `u64`.

---

# Example

    fn main() {
        // Arrays have a fixed size.
        // All elements are of the same type.
        let array = [1, 2, 3, 4, 5];

        // Create an array of 20 elements where all elements are the same.
        // The size should be a compile-time constant.
        let ones = [1; 20];

        // Get the length of an array.
        println!("Length of ones: {}", ones.len());

        // Access an element of an array.
        // Indexing starts at 0.
        println!("Second element of array: {}", array[1]);

        // Run-time bounds-check.
        // This panics with 'index out of bounds: the len is 5 but the index is 5'.
        println!("Non existant element of array: {}", array[5]);
    }

---

# Limitations

Pattern-matching on arrays (or slices) is not supported in stable Rust (see [#23121][1] and [slice patterns][2]).

Rust does not support genericity of type-level numerals (see [RFCs#1657][3]). Therefore, it is not possible to simply implement a trait for all arrays (of all sizes). As a result, the standard traits are only implemented for arrays up to a limited number of elements (last checked, up to 32 included). Arrays with more elements are supported, but do not implement the standard traits (see [docs][4]).

These restrictions will hopefully be lifted in the future.


  [1]: https://github.com/rust-lang/rust/issues/23121
  [2]: https://doc.rust-lang.org/book/slice-patterns.html
  [3]: https://github.com/rust-lang/rfcs/pull/1657
  [4]: https://doc.rust-lang.org/std/primitive.array.html

## Slices
Slices are views into a list of objects, and have type `[T]`, indicating a slice of objects with type `T`.

A slice is an [unsized type][1], and therefore can only be used behind a pointer.  _(String world analogy: `str`, called string slice, is also unsized.)_

Arrays get coerced into slices, and vectors can be dereferenced to slices. Therefore, slice methods can be applied to both of them. _(String world analogy: `str` is to `String`, what `[T]` is to `Vec<T>`.)_

    fn main() {
        let vector = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let slice = &vector[3..6];
        println!("length of slice: {}", slice.len()); // 3
        println!("slice: {:?}", slice); // [4, 5, 6]
    }


  [1]: https://doc.rust-lang.org/book/unsized-types.html

## Vectors
A vector is essentially a pointer to a heap-allocated, dynamically-sized list of objects of a single type.

---

# Example

    fn main() {
        // Create a mutable empty vector
        let mut vector = Vec::new();

        vector.push(20);
        vector.insert(0, 10); // insert at the beginning

        println!("Second element of vector: {}", vector[1]); // 20

        // Create a vector using the `vec!` macro
        let till_five = vec![1, 2, 3, 4, 5];

        // Create a vector of 20 elements where all elements are the same.
        let ones = vec![1; 20];

        // Get the length of a vector.
        println!("Length of ones: {}", ones.len());

        // Run-time bounds-check.
        // This panics with 'index out of bounds: the len is 5 but the index is 5'.
        println!("Non existant element of array: {}", till_five[5]);
    }

