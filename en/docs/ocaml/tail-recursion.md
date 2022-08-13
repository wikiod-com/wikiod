---
title: "Tail recursion"
slug: "tail-recursion"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Functional languages such as OCaml rely heavily on [recursive functions](https://en.wikipedia.org/wiki/Recursion_(computer_science)). However, such functions can lead to memory over consumption or, when handling large datasets, to [stack overflows](https://en.wikipedia.org/wiki/Stack_overflow). 

Tail recursion is an important source of optimization in such cases. It allows a program to drop the caller context **when the recursive call is the last of the function**.

## Sum function
Below is a non-tail-recursive function to compute the sum of a list of integers.

    let rec sum = function
      | [] -> 0
      | h::t -> h + (sum t)

The last operation the function performs is the addition. Thus, the function isn't tail-recursive.

Below is a tail-recursive version of the same function.

    let sum l =
      let rec aux acc = function
        | [] -> acc
        | h::t -> aux (acc+h) t
      in
      aux 0 l

Here, the `aux` function is tail-recursive: the last operation it performs is calling itself. As a consequence, the latter version of `sum` can be used with lists of any length.

