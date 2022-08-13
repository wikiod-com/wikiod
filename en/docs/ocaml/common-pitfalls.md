---
title: "Common Pitfalls"
slug: "common-pitfalls"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Using the wrong operator
In OCaml, there are different arithmetic operators for floats and integers. Additionally, these operators can only be used on 2 floats or 2 integers. Here are invalid expressions in OCaml

    1.0 + 2.0
    1 + 2.0
    1 +. 2
    1 +. 2.0
    
The correct expression for each of these respectively are
    
    1. +. 2.
    float_of_int 1 +. 2.
    1 + 2
    float_of_int 1 +. 2.

There is no automatic casting of integers to floats or vice-versa in OCaml. Everything is explicit. Here is a list of the integer and float operators

|Operation | Integer Operator | Float Operator|
| ------ | ------ | ------ |
|Addition | `a + b`   | `c +. d`   |
| Subtraction | `a - b` | `c -. d` |
| Multiplication | `a * b` | `c *. c` |
| Division | `a / b` | `c /. d` |
| Modulus | `a mod b` | `modfloat c d` |
| Exponentiation | N/a | `c ** d`
Where `a` and `b` are integers and `c` and `d` are floats.

## Forgetting parentheses around function arguments
A common mistake is to forget surrounding compound function arguments with parentheses, leading to type errors.

    # string_of_int 1+1;;

    Error: This expression has type string but an expression was expected of type int

This is because of the precedence. In fact, the above evaluates to

    # (string_of_int 1) + 1;;

which is wrong. A correct syntax would be

    # string_of_int (1+1);;

    - : string = "2"

