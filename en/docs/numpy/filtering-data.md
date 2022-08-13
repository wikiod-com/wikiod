---
title: "Filtering data"
slug: "filtering-data"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Filtering data with a boolean array
When only a single argument is supplied to numpy's `where` function it returns the indices of the input array (the `condition`) that evaluate as true (same behaviour as `numpy.nonzero`). This can be used to extract the indices of an array that satisfy a given condition.

```
import numpy as np

a = np.arange(20).reshape(2,10)
# a = array([[ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9],
#           [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]])

# Generate boolean array indicating which values in a are both greater than 7 and less than 13
condition = np.bitwise_and(a>7, a<13)
# condition = array([[False, False, False, False, False, False, False, False,  True, True],
#                    [True,  True,  True, False, False, False, False, False, False, False]], dtype=bool)

# Get the indices of a where the condition is True
ind = np.where(condition)
# ind = (array([0, 0, 1, 1, 1]), array([8, 9, 0, 1, 2]))

keep = a[ind]
# keep = [ 8  9 10 11 12]
```
If you do not need the indices, this can be achieved in one step using `extract`, where you agian specify the `condition` as the first argument, but give the `array` to return the values from where the condition is true as the second argument.

```
# np.extract(condition, array)
keep = np.extract(condition, a)
# keep = [ 8  9 10 11 12]
```
Two further arguments `x` and `y` can be supplied to `where`, in which case the output will contain the values of `x` where the condition is `True` and the values of `y` where the condition is `False`. 
```
# Set elements of a which are NOT greater than 7 and less than 13 to zero, np.where(condition, x, y)
a = np.where(condition, a, a*0)
print(a)
# Out: array([[ 0,  0,  0,  0,  0,  0,  0,  0,  8,  9],
#            [10, 11, 12,  0,  0,  0,  0,  0,  0,  0]])
```

## Directly filtering indices
For simple cases, you can filter data directly.

    a = np.random.normal(size=10)
    print(a)
    #[-1.19423121  1.10481873  0.26332982 -0.53300387 -0.04809928  1.77107775
    # 1.16741359  0.17699948 -0.06342169 -1.74213078]
    b = a[a>0]
    print(b)
    #[ 1.10481873  0.26332982  1.77107775  1.16741359  0.17699948]


