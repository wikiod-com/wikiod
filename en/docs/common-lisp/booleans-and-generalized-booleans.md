---
title: "Booleans and Generalized Booleans"
slug: "booleans-and-generalized-booleans"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## True and False
The special symbol `T` represents the value *true* in Common Lisp, while the special symbol `NIL` represents *false*:

    CL-USER> (= 3 3)
    T
    CL-USER> (= 3 4)
    NIL

They are called “Constant Variables” (sic!) in the standard, since they are variables whose value *cannot* be modified. As a consequence, you cannot use their names for normal variables, like in the following, incorrect, example:

    CL-USER> (defun my-fun(t)
               (+ t 1))
    While compiling MY-FUN :
    Can't bind or assign to constant T.

Actually, one can consider them simply as constants, or as self-evaluated symbols. `T` and `NIL` are specials in other senses, too. For instance, `T` is also a type (the supertype of any other type), while `NIL` is also the empty list:

    CL-USER> (eql NIL '())
    T
    CL-USER> (cons 'a (cons 'b nil))
    (A B)



## Generalized Booleans
Actually any value different from `NIL` is considered a *true* value in Common Lisp. For instance:

    CL-USER> (let ((a (+ 2 2)))
               (if a
                   a
                   "Oh my! 2 + 2 is equal to NIL!"))
    4

This fact can be combined with the boolean operators to make programs more concise. For instance, the above example is equivalent to:

    CL-USER> (or (+ 2 2) "Oh my! 2 + 2 is equal to NIL!")
    4

The macro `OR` evaluates its arguments in order from left to right and stops as soon as it finds a non-NIL value, returning it. If all of them are `NIL`, the value returned is `NIL`:

    CL-USER> (or (= 1 2) (= 3 4) (= 5 6))
    NIL

Analogously, the macro `AND` evaluates its arguments from left to right and returns the value of the last, if all of them are evaluated to non-NIL, otherwise stops the evaluation as soon as it finds `NIL`, returning it:

    CL-USER> (let ((a 2)
                   (b 3))
               (and (/= b 0) (/ a b)))
    2/3
    CL-USER> (let ((a 2)
                   (b 0))
               (and (/= b 0) (/ a b)))
    NIL

For these reasons, `AND` and `OR` can be considered more similar to control structures of other languages, rather than to boolean operators.



