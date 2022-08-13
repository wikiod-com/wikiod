---
title: "numpy.cross"
slug: "numpycross"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- `numpy.cross(a, b)`  # cross product of *a* and *b* (or vectors in *a* and *b*)
- `numpy.cross(a, b, axisa=-1)`  #cross product of vectors in *a* with *b*, s.t. vectors in a are laid out along axis *axisa*
- `numpy.cross(a, b, axisa=-1, axisb=-1, axisc=-1)` # cross products of vectors in *a* and *b*, output vectors laid out along axis specified by *axisc*
- `numpy.cross(a, b, axis=None)`  # cross products of vectors in *a* and *b*, vectors in *a*, *b*, and in output laid out along axis *axis*


## Parameters



| Column | Column |
| ------ | ------ |
| a,b    | In simplest usage, `a` and `b` are two 2- or 3-element vectors.  They can also be arrays of vectors (i.e. two-dimensional matrices).  If `a` is an array and 'b' is a vector, `cross(a,b)` returns an array whose elements are the cross products of each vector in `a` with the vector `b`.  The `b` is an array and `a` is a single vector, `cross(a,b)` returns an array whose elements are the cross products of `a` with each vector in `b`.  `a` and `b` can both be arrays if they have the same shape.  In this case, `cross(a,b)` returns `cross(a[0],b[0]), cross(a[1], b[1]), ...`|
|axisa/b|If `a` is an array, it can have vectors laid out across the most quickly varying axis, the slowest varying axis, or something in between.  `axisa` tells `cross()` how the vectors are laid out in `a`.  By default, it takes the value of the most slowly varying axis.  `axisb` works the same with input `b`.  If the output of `cross()` is going to be an array, the output vectors can be laid out different array axes; `axisc` tells `cross` how to lay out the vectors in its output array.  By default, `axisc` indicates the most slowly varying axis.|
|axis| A convenience parameter that sets `axisa`, `axisb`, and `axisc` all to the same value if desired.  If `axis` and any of the other parameters are present in the call, the value of `axis` will override the other values.

## Cross Product of Two Vectors
Numpy provides a [`cross`][1] function for computing vector cross products. The cross product of vectors `[1, 0, 0]` and `[0, 1, 0]` is `[0, 0, 1]`. Numpy tells us:

    >>> a = np.array([1, 0, 0])
    >>> b = np.array([0, 1, 0])
    >>> np.cross(a, b)
    array([0, 0, 1])

as expected.

While cross products are normally defined only for three dimensional vectors. However, either of the arguments to the Numpy function can be two element vectors. If vector `c` is given as `[c1, c2]`, Numpy assigns zero to the third dimension: `[c1, c2, 0]`. So,

    >>> c = np.array([0, 2])
    >>> np.cross(a, c)
    array([0, 0, 2])

Unlike `dot` which exists as both a [Numpy function][2] and a [method of `ndarray`][3], `cross` exists only as a standalone function:

    >>> a.cross(b)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    AttributeError: 'numpy.ndarray' object has no attribute 'cross'


  [1]: https://docs.scipy.org/doc/numpy/reference/generated/numpy.cross.html
  [2]: https://docs.scipy.org/doc/numpy/reference/generated/numpy.dot.html
  [3]: https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.dot.html

## Multiple Cross Products with One Call
Either input can be an array of 3- (or 2-) element vectors.

    >>> a=np.array([[1,0,0],[0,1,0],[0,0,1]])
    >>> b=np.array([1,0,0])
    >>> np.cross(a,b)
    array([[ 0,  0,  0],
           [ 0,  0, -1],
           [ 0,  1,  0]])

The result in this case is array([np.cross(a[0],b), np.cross(a[1],b), np.cross(a[2],b)])

`b` can also be an array of 3- (or 2-) element vectors, but it must have the same shape as `a`. Otherwise the calculation fails with a "shape mismatch" error. So we can have

    >>> b=np.array([[0,0,1],[1,0,0],[0,1,0]])
    >>> np.cross(a,b)
    array([[ 0, -1,  0],
           [ 0,  0, -1],
           [-1,  0,  0]])

and now the result is `array([np.cross(a[0],b[0]), np.cross(a[1],b[1]), np.cross(a[2],b[2])])`


## More Flexibility with Multiple Cross Products
In our last two examples, numpy assumed that `a[0,:]` was the first vector, `a[1,:]` the second, and `a[2,:]` the third. Numpy.cross has an optional argument axisa that allows us to specify which axis defines the vectors. So,

    >>> a=np.array([[1,1,1],[0,1,0],[1,0,-1]])
    >>> b=np.array([0,0,1])
    >>> np.cross(a,b)
    array([[ 1, -1,  0],
           [ 1,  0,  0],
           [ 0, -1,  0]])
    >>> np.cross(a,b,axisa=0)
    array([[ 0, -1,  0],
           [ 1, -1,  0],
           [ 0, -1,  0]])
    >>> np.cross(a,b,axisa=1)
    array([[ 1, -1,  0],
           [ 1,  0,  0],
           [ 0, -1,  0]])

The `axisa=1` result and the default result are both `(np.cross([1,1,1],b), np.cross([0,1,0],b), np.cross([1,0,-1],b))`. By default, `axisa` always indicates the last (most slowly varying) axis of the array. The `axisa=0` result is `(np.cross([1,0,1],b), np.cross([1,1,0],b), np.cross([1,0,-1],b))`.

A similar optional parameter, `axisb`, performs the same function for the `b` input, if it is also a 2-dimensional array.

Parameters axisa and axisb tell numpy how to distribute the input data. A third parameter, axisc tells numpy how to distribute the output if `a` or `b` is multi-dimensional. Using the same inputs `a` and `b` as above, we get

    >>> np.cross(a,b,1)
    array([[ 1, -1,  0],
           [ 1,  0,  0],
           [ 0, -1,  0]])
    >>> np.cross(a,b,1,axisc=0)
    array([[ 1,  1,  0],
           [-1,  0, -1],
           [ 0,  0,  0]])
    >>> np.cross(a,b,1,axisc=1)
    array([[ 1, -1,  0],
           [ 1,  0,  0],
           [ 0, -1,  0]])

So `axisc=1` and the default `axisc` both give the same result, that is, the elements of each vector are contiguous in the fastest moving index of the output array. axisc is by default the last axis of the array. `axisc=0` distributes the elements of each vector across the slowest varying dimension of the array.

If you want `axisa`, `axisb`, and `axisc` to all have the same value, you do not need to set all three parameters. You can set a fourth parameter, `axis`, to the needed single value and the other three parameters will be automatically set. axis overrides axisa, axisb, or axisc if any of them are present in the function call.

