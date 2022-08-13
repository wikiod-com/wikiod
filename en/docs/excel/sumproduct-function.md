---
title: "SUMPRODUCT function"
slug: "sumproduct-function"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

The SUMPRODUCT function multiplies corresponding components in the given arrays, and returns the sum of those products

## Syntax
 - SUMPRODUCT(array1, [array2], [array3], ...)

 - The array arguments must have the same dimensions. If they do not, SUMPRODUCT returns the #VALUE! error value.
 - SUMPRODUCT treats array entries that are not numeric as if they were zeros.

## Using SUMPRODUCT with boolean arrays
Consider the following ranges `A1:A3` and `B1:B3` as below <br/><br/>
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/f9GEr.png

    =SUMPRODUCT(--(A1:A3="c"),B1:B3)

This will first manipulate `(A1:A3="c")` into the following array

    A1="c" = FALSE
    A2="c" = FALSE
    A3="c" = TRUE

Then apply the `--` operator which converts `TRUE` and `FALSE` into `1` and `0`, respectively. So the array becomes

    --FALSE = 0
    --FALSE = 0
    --TRUE  = 1

Then the SUMPRODUCT formula completes as in the simple numeric case. Returning `6` in this example

    0*4 = 0
    0*5 = 0
    1*6 = 6

*Note: this is the equivalent of a SUMIF function*


## Using SUMPRODUCT with numeric ranges
Consider the ranges `A1:A3` and `B1:B3` having the same size and only number values, as below<br/><br/>
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/RwgD2.png

    =SUMPRODUCT(A1:A3,B1:B3)

This will  loop through the ranges, taking the product of values in the same row and summing them, returning `32` in this example.

    A1*B1 = 4
    A2*B2 = 10
    A3*B3 = 18



