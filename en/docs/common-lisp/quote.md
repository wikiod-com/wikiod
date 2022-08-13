---
title: "Quote"
slug: "quote"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 - (quote object) -> object

There are some objects (for example keyword symbols) that don't need to be quoted since they evaluate to themselves.


## If quoted objects are destructively modified, the consequences are undefined!
Avoid destructive operations on quoted objects. Quoted objects are literal objects. They are possibly embedded in the code in some way. How this works and the effects of modifications are unspecified in the Common Lisp standard, but it can have unwanted consequences like modifying shared data, trying to modify write protected data or creating unintended side-effects.

<s>`(delete 5 '(1 2 3 4 5))`</s>

## Simple quote example
Quote is a **special operator** that prevents evaluation of its argument. It returns its argument, unevaluated.

    CL-USER> (quote a)
    A

    CL-USER> (let ((a 3))
               (quote a))
    A

## ' is an alias for the special operator QUOTE
The notation `'thing` is equal to `(quote thing)`.

The *reader* will do the expansion:

    > (read-from-string "'a")
    (QUOTE A)

Quoting is used to prevent further evaluation. The quoted object evaluates to itself.

    > 'a
    A

    > (eval '+ 1 2)
    3

## Quote and self-evaluating objects
Note that many datatypes don't need to be quoted, since they evaluate to themselves. `QUOTE` is especially useful for symbols and lists, to prevent evaluation as Lisp forms.

Example for other datatypes not needed to be quoted to prevent evaluation: strings, numbers, characters, CLOS objects, ...

Here an example for strings. The evaluation results are strings, whether they are quoted in the source or not.

    > (let ((some-string-1 "this is a string")
            (some-string-2 '"this is a string with a quote in the source")
            (some-string-3 (quote "this is another string with a quote in the source")))
        (list some-string-1 some-string-2 some-string-3))

    ("this is a string"
     "this is a string with a quote in the source"
     "this is another string with a quote in the source")

Quoting for the objects thus is optional.

