---
title: "Boolean Indexing"
slug: "boolean-indexing"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Creating a boolean array
A boolean array can be created manually by using `dtype=bool` when creating the array.  Values other than `0`, `None`, `False` or empty strings are considered True.
    
    import numpy as np
    
    bool_arr = np.array([1, 0.5, 0, None, 'a', '', True, False], dtype=bool)
    print(bool_arr)
    # output: [ True  True False False  True False  True False]

Alternatively, numpy automatically creates a boolean array when comparisons are made between arrays and scalars or between arrays of the same shape.

    arr_1 = np.random.randn(3, 3)
    arr_2 = np.random.randn(3, 3)

    bool_arr = arr_1 < 0.5    
    print(bool_arr.dtype)
    # output: bool

    bool_arr = arr_1 < arr_2
    print(bool_arr.dtype)
    # output: bool

