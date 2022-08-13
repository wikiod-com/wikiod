---
title: "Cons cells and lists"
slug: "cons-cells-and-lists"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## What is a cons cell?
A cons cell, also known as a dotted pair (because of its printed representation), is simply a pair of two objects.  A cons cell is created by the function `cons`, and elements in the pair are extracted using the functions `car` and `cdr`.

    (cons "a" 4)

For instance, this returns a pair whose first element (which can be extracted with `car`) is `"a"`, and whose second element (which can be extracted with `cdr`), is `4`.

    (car (cons "a" 4))
    ;;=> "a"
    
    (cdr (cons "a" 4))
    ;;=> 3

Cons cells can be printed in *dotted pair* notation:

    (cons 1 2)
    ;;=> (1 . 2)

Cons cells can also be read in dotted pair notation, so that

    (car '(x . 5))
    ;;=> x

    (cdr '(x . 5))
    ;;=> 5

(The printed form of cons cells can be a bit more complicated, too.  For more about that, see the example about cons cells as lists.)

That's it;  cons cells are just pairs of elements created by the function `cons`, and the elements can be extracted with `car` and `cdr`. Because of their simplicity, cons cells can be a useful building block for more complex data structures.

## Lists as a convention
Some languages include a list data structure.  Common Lisp, and other languages in the Lisp family, make extensive use of lists (and the name Lisp is based on the idea of a LISt Processor).  However, Common Lisp doesn't actually include a primitive list datatype.  Instead, lists exist by convention.  The convention depends on two principles:

1. The symbol **nil** is the empty list.
2. A non empty list is a *cons cell* whose *car* is the first element of the list, and whose *cdr* is the rest of the list.

That's all that there is to lists.  If you've read the example called *What is a cons cell?*, then you know that a cons cell whose car is X and whose cdr is Y can be written as **(X . Y)**.  That means that we can write some lists based on the principles above.   The list of the elements 1, 2, and 3 is simply:

    (1 . (2 . (3 . nil)))

However, because lists are so common in the Lisp family of languages, there are special printing conventions beyond the simple dotted pair notation for cons cells.  

1. The symbol **nil** can also be written as **()**.
2. When the cdr of one cons cell is another list (either **()** or a cons cell), instead of writing the one cons cell with the dotted pair notation, the "list notation" is used.

The list notation is shown most clearly by several examples:

    (x . (y . z))   === (x y . z)
    (x . NIL)       === (x)
    (1 . (2 . NIL)) === (1 2)
    (1 . ())        === (1)

The idea is that the elements of the list are written in successive order within parenthesis until the final cdr in the list is reached.  If the final cdr is **nil** (the empty list), then the final parenthesis is written.  If the final cdr is not **nil** (in which case the list is called an *improper list*), then a dot is written, and then that final cdr is written.







## Sketching cons cells
To better understand the semantics of conses and lists, a graphical representation of this kind of structures is often used. A cons cell is usually represented with two boxes in contact, that contain either two arrows that point to the `car` and `cdr` values, or directly the values. For instance, the result of:

    (cons 1 2)   
    ;; -> (1 . 2)

can be represented with one of these drawings:

<img src="http://i.stack.imgur.com/VzBXN.png" width="400" height="80">

Note that these representations are purely conceptual, and do not denote the fact that the values are *contained* into the cell, or are *pointed* from the cell: in general this depends on the implementation, the type of the values, the level of optimization, etc. In the rest of the example we will use the first kind of drawing, which is the one more commonly used.

So, for instance:

    (cons 1 (cons 2 (cons 3 4)))   ; improper “dotted” list
    ;; -> (1 2 3 . 4)

is represented as:

<img src="http://i.stack.imgur.com/nFaj3.png" width="360" height="100">

while:

    (cons 1 (cons 2 (cons 3 (cons 4 nil))))  ;; proper list, equivalent to: (list 1 2 3 4)
    ;; -> (1 2 3 4)

is represented as:

<img src="http://i.stack.imgur.com/iG6DE.png" width="480" height="100">

Here is a tree-like structure:

    (cons (cons 1 2) (cons 3 4))
    ;; -> ((1 . 2) 3 . 4)         ; note the printing as an improper list

<img src="http://i.stack.imgur.com/L16gN.png" width="280" height="160">

The final example shows how this notation can help us to understand important semantics aspects of the language. First, we write an expression similar to the previous one:

    (cons (cons 1 2) (cons 1 2))
    ;; -> ((1 . 2) 1 . 2)

that can be represented in the usual way as:

<img src="http://i.stack.imgur.com/sxnmv.png" width="280" height="160">

Then, we write a different expression, which is apparently equivalent to the previous one, and this seems confirmed by printed representation of the result:

    (let ((cell-a (cons 1 2)))
      (cons cell-a cell-a))
    ;; -> ((1 . 2) 1 . 2)

But, if we draw the diagram, we can see that the semantics of the expression is different, since the *same* cell is the value *both* of the `car` part and the `cdr` part of the outer `cons` (this is, `cell-a` is *shared*):

<img src="http://i.stack.imgur.com/V34Fg.png" width="130" height="160">

and the fact that the semantics of the two results is actually different at the language level can be verified by the following tests:

    (let ((c1 (cons (cons 1 2) (cons 1 2)))
          (c2 (let ((cell-a (cons 1 2)))
                (cons cell-a cell-a))))
      (list (eq (car c1) (cdr c1))
            (eq (car c2) (cdr c2)))
    ;; -> (NIL T)

The first `eq` is *false* since the `car` and `cdr` of `c1` are structurally equal (that is *true* by `equal`), but are not “identical” (i.e. “the same shared structure”), while in the second test the result is *true* since the `car` and `cdr` of `c2` are *identical*, that is they are *the same structure*.

