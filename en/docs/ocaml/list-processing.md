---
title: "List Processing"
slug: "list-processing"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## List.Map
`List.map` has the signature `('a -> 'b) -> 'a list -> 'b list` which in English is a function that takes a function (we'll call this the mapping function) from one type (namely `'a`) to another type (namely `'b`) and a list of the first type. The function returns a list of the second type where every element is the result of calling the mapping function on an element of the first list.

    List.map string_of_int [ 1; 2; 3; 4 ]
    #- [ "1"; "2"; "3"; "4" ] : string list

The types `'a` and `'b` don't have to be different. For example, we can map numbers to their squares just as easily.

    let square x = x * x in
    List.map square [ 1; 2; 3; 4 ]
    #- [ 1; 4; 9; 16 ] : int list

## Aggregate data in a list
The `List.fold_left` and `List.fold_right` functions are [higher-order](https://www.wikiod.com/ocaml/higher-order-functions) functions that implement the outer logic of list aggregation.  Aggregating a list, sometimes also referred to as reducing a list, means computing a value derived from the sequential inspection of all items in that list.

The [documentation of the List module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html) states that

- `List.fold_left f a [b1; ...; bn]` is `f (... (f (f a b1) b2) ...) bn`.
- `List.fold_right f [a1; ...; an] b` is `f a1 (f a2 (... (f an b) ...))`. (This latter function is not tail-recursive.)

In plain English computing `List.fold_left f a [b1; ...; bn]` amounts to running through the list `[b1; ...; bn]` keeping track of an *accumulator* initially set to `a`: each time we see an item in the list, we use `f` to update the value of the accumulator, and when we are done, the accumulator is the final value of our computation.  The `List.fold_right` function is similar.

Here are a few practical examples:


### Compute the total sum of a list of numbers

    List.fold_left ( + ) 0 lst

### Compute the average of a list of floats

    let average lst =
      let (sum, n) =
        List.fold_left (fun (sum, n) x -> (sum +. x, n + 1)) (0.0, 0) lst
      in
      sum /. (float_of_int n)

### Re-implement basic list processing

The functions `List.fold_left` and `List.fold_right` are so general that they can be used to implement almost every other functions from the list module:

    let list_length lst = (* Alternative implementation to List.length *)
      List.fold_left ( + ) 0 lst

    let list_filter predicate lst = (* Alternative implementation to List.filter *)
      List.fold_right (fun a b -> if predicate a then a :: b else b) lst []

It is even possible to reimplement the `List.iter` function, remember that `()` is the global state of the program to interpret this code as a further example of *list aggregation*:

    let list_iter f lst = (* Alternation implementation to List.iter *)
      List.fold_left (fun () b -> f b) () lst

These examples are meant to be learning material, these implementations have no virtue over the corresponding functions from the standard library.

