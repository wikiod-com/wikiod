---
title: "Data Structures"
slug: "data-structures"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
- [head | tail] = [1, 2, 3, true] # one can use pattern matching to break up cons cells. This assigns head to 1 and tail to [2, 3, true]

- %{d: val} = %{d: 1, e: true} \# this assigns val to 1; no variable d is created because the d on the lhs is really just a symbol that is used to create the pattern %{:d => _} (note that hash rocket notation allows one to have non-symbols as keys for maps just like in ruby)

As for which data structure to us here are some brief remarks. 

If you need an array data structure if you're going to be doing a lot of writing use lists. If instead you are going to be doing a lot of read you should use tuples.

As for maps they are just simply how you do key value stores. 

## Lists
    a = [1, 2, 3, true]
Note that these are stored in memory as linked lists. Id est this is a series of cons cells where the head (List.hd/1) is the value of first item of the list and the tail (List.tail/1) is the value of the rest of the list. 

    List.hd(a) = 1
    List.tl(a) = [2, 3, true]

## Tuples
    b = {:ok, 1, 2}
Tuples are the equivalent of arrays in other languages. They are stored contiguously in memory.

