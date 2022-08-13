---
title: "Intrinsic procedures"
slug: "intrinsic-procedures"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Many of the available intrinsic procedures have argument types in common.  For example:

  - a logical argument `MASK` which selects elements of input arrays to be processed
  - an integer scalar argument `KIND` which determines the kind of the function result
  - an integer argument `DIM` for a reduction function which controls the dimension over which the reduction is performed

## Using PACK to select elements meeting a condition
The intrinsic `pack` function packs an array into a vector, selecting elements based on a given mask.  The function has two forms

    PACK(array, mask)
    PACK(array, mask, vector)

(that is, the `vector` argument is optional).

In both cases `array` is an array, and `mask` of logical type and conformable with `array` (either a scalar or an array of the same shape).

In the first case the result is rank-1 array of type and type parameters of `array` with the number of elements being the number of true elements in the mask.

    integer, allocatable :: positive_values(:)
    integer :: values(5) = [2, -1, 3, -2, 5]
    positive_values = PACK(values, values>0)

results in `positive_values` being the array `[2, 3, 5]`.

With the `vector` rank-1 argument present the result is now the size of `vector` (whcih must have at least as many elements as there are true values in `mask`.

The effect with `vector` is to return that array with the initial elements of that array overwritten by the masked elements of `array`.  For example

    integer, allocatable :: positive_values(:)
    integer :: values(5) = [2, -1, 3, -2, 5]
    positive_values = PACK(values, values>0, [10,20,30,40,50])

results in `positive_values` being the array `[2,3,5,40,50]`.

It should be noted that, regardless of the shape of the argument `array` the result is always a rank-1 array.

---

In addition to selecting the elements of an array meeting a masking condition it is often useful to determine the indices for which the masking condition is met.  This common idiom can be expressed as

    integer, allocatable :: indices(:)
    integer i
    indices = PACK([(i, i=1,5)], [2, -1, 3, -2, 5]>0)

resulting in `indices` being the array `[1,3,5]`.

