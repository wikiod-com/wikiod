---
title: "Pairs"
slug: "pairs"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

A Pair is one of the most basic data type in scheme. It is also usually called cons cells.

## Create a pair
A pair can be create with the `cons` function. The name of the function stand for _constructor_. In Scheme, everything is pretty much based on pairs.

    (cons a b)

The function return a pair containing the element `a` and `b`. The first parameter of `cons` is called `car` (Content Address Register) and the second argument is the `cdr` (Content Decrement Register).

## Access the car of the pair.
The data in the pair can be accessed with utility functions. To access the `car`, we have to use the `car` function.

    (car (cons a b))
    > a

Also we can verify the following equality:

    (eq? a (car (cons a b)))
    > #t

## Access the cdr of the pair
To access the cdr, we have to use the cdr function.

(cdr (cons a b))
> b

Also we can verify the following equality:

(eq? b (cdr (cons a b)))
> #t

## Create a list with pairs
List in scheme are nothing else than a series of pairs nested in each other in the `cdr` of a `cons`. And the last `cdr` of a proper list is the empty list `'()`.

To create the list `(1 2 3 4)`, we'd have something like this:

    (cons 4 '())
    > (4)
    (cons 3 (cons 4 '()))
    > (3 4)
    (cons 2 (cons 3 (cons 4 '())))
    > (2 3 4)
    (cons 1 (cons 2 (cons 3 (cons 4 '()))))
    > (1 2 3 4)

As you can see, a list in scheme is a linked list made out of pairs. For that reason, adding an object to the front of the list takes almost no time, but appending an element at the end of the list forces the interpreter to walk accross the whole list.

