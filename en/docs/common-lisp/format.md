---
title: "format"
slug: "format"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Parameters

| Lambda-List| `(format DESTINATION CONTROL-STRING &REST FORMAT-ARGUMENTS)` |
| ------ | ------ |
| `DESTINATION`  |  the thing to write to. This can be an output stream, `t` (shorthand for `*standard-output*`), or `nil` (which creates a string to write to)  |
| `CONTROL-STRING` | the template string. It might be a primitive string, or it might contain tilde-prefixed command directives that specify, and somehow transform additional arguments. |
| `FORMAT-ARGUMENTS` | potential additional arguments required by the given `CONTROL-STRING`.|

The CLHS documentation for `FORMAT` directives can be found in [Section 22.3](http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm). With SLIME, you can type `C-c C-d ~` to look up the CLHS documentation for a specific format directive.

## Basic Usage and Simple Directives
The first two arguments to format are an output stream and a control string. Basic use does not require additional arguments. Passing `t` as the stream writes to `*standard-output*`.

    > (format t "Basic Message")
    Basic Message
    nil

That expression will write `Basic Message` to standard output, and return `nil`.

Passing `nil` as the stream creates a new string, and returns it.

    > (format nil "Basic Message")
    "Basic Message"

Most control string directives require additional arguments. The `~a` directive ("aesthetic") will print any argument as though by the `princ` procedure. This prints the form without any escape characters (keywords are printed without the leading colon, strings without their surrounding quotes and so forth).

    > (format nil "A Test: ~a" 42)
    "A Test: 42"
    > (format nil "Multiples: ~a ~a ~a ~a" 1 (list 2 3) "four five" :six)
    "Multiples: 1 (2 3) four five SIX"
    > (format nil "A Test: ~a" :test)
    "A Test: TEST"
    > (format nil "A Test: ~a" "Example")
    "A Test: Example"

`~a` optionally right or left-pads input based on additional inputs.

    > (format nil "A Test: ~10a" "Example")
    "A Test: Example   "
    > (format nil "A Test: ~10@a" "Example")
    "A Test:    Example"

The `~s` directive is like `~a`, but it prints escape characters.

    > (format nil "A Test: ~s" 42)
    "A Test: 42"
    > (format nil "Multiples: ~s ~s ~s ~s" 1 (list 2 3) "four five" :six)
    "Multiples: 1 (2 3) \"four five\" :SIX"
    > (format nil "A Test: ~s" :test)
    "A Test: :TEST"
    > (format nil "A Test: ~s" "Example")
    "A Test: \"Example\""

## Iterating over a list
One can iterate over a list using [`~{`](http://www.lispworks.com/documentation/HyperSpec/Body/22_cgd.htm) and [`~}`](http://www.lispworks.com/documentation/HyperSpec/Body/22_cge.htm) directives.

    CL-USER> (format t "~{~a, ~}~%" '(1 2 3 4 5))
    1, 2, 3, 4, 5, 

[`~^`](http://www.lispworks.com/documentation/HyperSpec/Body/22_cib.htm) can be used to escape if there are no more elements left.

    CL-USER> (format t "~{~a~^, ~}~%" '(1 2 3 4 5))
    1, 2, 3, 4, 5

A numeric argument can be given to `~{` to limit how many iterations can be done:

    CL-USER> (format t "~3{~a~^, ~}~%" '(1 2 3 4 5))
    1, 2, 3, 

`~@{` will iterate over remaining arguments, instead of a list:

    CL-USER> (format t "~a: ~@{~a~^, ~}~%" :foo 1 2 3 4 5)
    FOO: 1, 2, 3, 4, 5

Sublists can be iterated over by using `~:{`:

    CL-USER> (format t "~:{(~a, ~a) ~}~%" '((1 2) (3 4) (5 6)))
    (1, 2) (3, 4) (5, 6) 

## Conditional expressions
Conditional expressions can be done with [`~[`](http://www.lispworks.com/documentation/HyperSpec/Body/22_cgb.htm) and [`~]`](http://www.lispworks.com/documentation/HyperSpec/Body/22_cgc.htm). The clauses of the expression are separated using [`~;`](http://www.lispworks.com/documentation/HyperSpec/Body/22_cia.htm).

By default, `~[` takes an integer from the argument list, and picks the corresponding clause. The clauses start at zero.

    (format t "~@{~[First clause~;Second clause~;Third clause~;Fourth clause~]~%~}"
            0 1 2 3)
    ; First clause
    ; Second clause
    ; Third clause
    ; Fourth clause

The last clause can be separated with `~:;` instead to make it the else-clause.

    (format t "~@{~[First clause~;Second clause~;Third clause~:;Too high!~]~%~}"
            0 1 2 3 4 5)
    ; First clause
    ; Second clause
    ; Third clause
    ; Too high!
    ; Too high!
    ; Too high!

If the conditional expression starts with `~:[`, it will expect a [generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean) instead of an integer. It can only have two clauses; the first one is printed if the boolean was `NIL`, and the second clause if it was truthy.

    (format t "~@{~:[False!~;True!~]~%~}"
            t nil 10 "Foo" '())
    ; True!
    ; False!
    ; True!
    ; True!
    ; False!

If the conditional expression starts with `~@[`, there should only be one clause, which is printed if the input, a generalized boolean, was truthy. The boolean will not be consumed if it is truthy.

    (format t "~@{~@[~s is truthy!~%~]~}"
            t nil 10 "Foo" '())
    ; T is truthy!
    ; 10 is truthy!
    ; "Foo" is truthy!

