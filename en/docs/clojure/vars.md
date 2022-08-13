---
title: "Vars"
slug: "vars"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - (def symbol value)
 - (def symbol "docstring" value)
 - (declare symbol_0 symbol_1 symbol_2 ...)

This should not be confused with `(defn)`, which is used for defining functions.

## Types of Variables
There are different types of variables in Clojure:

 - numbers
    
    Types of numbers:
    
    - integers
    - longs (numbers larger than `2^31 - 1`)
    - floats (decimals)

 - strings
 - collections

    Types of collections:
    - maps
    - sequences
    - vectors

 - functions

