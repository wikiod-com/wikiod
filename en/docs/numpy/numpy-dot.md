---
title: "numpy.dot"
slug: "numpydot"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

## Syntax
 - numpy.dot(a, b, out=None)

## Parameters


| Name | Details |
| ------ | ------ |
| a   | a numpy array   |
| b   | a numpy array   |
| out   | a numpy array   |


**numpy.dot**


Returns the dot product of a and b. If a and b are both scalars or both 1-D arrays then a scalar is returned; otherwise an array is returned. If out is given, then it is returned.

## Matrix multiplication
Matrix multiplication can be done in two equivalent ways with the dot function. One way is to use the dot member function of numpy.ndarray.

    >>> import numpy as np
    >>> A = np.ones((4,4))
    >>> A
    array([[ 1.,  1.,  1.,  1.],
           [ 1.,  1.,  1.,  1.],
           [ 1.,  1.,  1.,  1.],
           [ 1.,  1.,  1.,  1.]])
    >>> B = np.ones((4,2))
    >>> B
    array([[ 1.,  1.],
           [ 1.,  1.],
           [ 1.,  1.],
           [ 1.,  1.]])
    >>> A.dot(B)
    array([[ 4.,  4.],
           [ 4.,  4.],
           [ 4.,  4.],
           [ 4.,  4.]])

The second way to do matrix multiplication is with the numpy library function.     

    >>> np.dot(A,B)
    array([[ 4.,  4.],
           [ 4.,  4.],
           [ 4.,  4.],
           [ 4.,  4.]])

## Vector dot products
The dot function can also be used to compute vector dot products between two one-dimensional numpy arrays.

    >>> v = np.array([1,2])
    >>> w = np.array([1,2])
    >>> v.dot(w)
    5
    >>> np.dot(w,v)
    5
    >>> np.dot(v,w)
    5



## The out parameter
The numpy dot function has an optional parameter out=None. This parameter allows you to specify an array to write the result to. This array must be exactly the same shape and type as the array that would have been returned, or an exception will be thrown.

    >>> I = np.eye(2)
    >>> I
    array([[ 1.,  0.],
           [ 0.,  1.]])
    >>> result = np.zeros((2,2))
    >>> result
    array([[ 0.,  0.],
           [ 0.,  0.]])
    >>> np.dot(I, I, out=result)
    array([[ 1.,  0.],
           [ 0.,  1.]])
    >>> result
    array([[ 1.,  0.],
           [ 0.,  1.]])

Let's try changing the dtype of result to int.

    >>> np.dot(I, I, out=result)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    ValueError: output array is not acceptable (must have the right type, nr dimensions, and be a C-Array)

And if we try using a different underlying memory order, say Fortran-style (so columns are contiguous instead of rows), an  error also results.

    >>> result = np.zeros((2,2), order='F')
    >>> np.dot(I, I, out=result)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    ValueError: output array is not acceptable (must have the right type, nr dimensions, and be a C-Array)

   




## Matrix operations on arrays of vectors
numpy.dot can be used to multiply a list of vectors by a matrix but the orientation of the vectors must be vertical so that a list of eight two component vectors appears like two eight components vectors:

    >>> a
    array([[ 1.,  2.],
           [ 3.,  1.]])
    >>> b
    array([[  1.,   2.,   3.,   4.,   5.,   6.,   7.,   8.],
           [  9.,  10.,  11.,  12.,  13.,  14.,  15.,  16.]])
    >>> np.dot(a, b)
    array([[ 19.,  22.,  25.,  28.,  31.,  34.,  37.,  40.],
           [ 12.,  16.,  20.,  24.,  28.,  32.,  36.,  40.]])

If the list of vectors is laid out with its axes the other way round (which is often the case) then the array needs to be transposed before and then after the dot operation like this:

    >>> b
    array([[  1.,   9.],
           [  2.,  10.],
           [  3.,  11.],
           [  4.,  12.],
           [  5.,  13.],
           [  6.,  14.],
           [  7.,  15.],
           [  8.,  16.]])
    >>> np.dot(a, b.T).T
    array([[ 19.,  12.],
           [ 22.,  16.],
           [ 25.,  20.],
           [ 28.,  24.],
           [ 31.,  28.],
           [ 34.,  32.],
           [ 37.,  36.],
           [ 40.,  40.]])

Although the dot function is very fast, sometimes it is better to use einsum. The equivalent of the above would be:

    >>> np.einsum('...ij,...j', a, b)

Which is a little slower but allows a list of vertices to be multiplied by a corresponding list of matrices. This would be a very convoluted process using dot:

    >>> a
    array([[[ 0,  1],
            [ 2,  3]],
           [[ 4,  5],
            [ 6,  7]],
           [[ 8,  9],
            [10, 11]],
           [[12, 13],
            [14, 15]],
           [[16, 17],
            [18, 19]],
           [[20, 21],
            [22, 23]],
           [[24, 25],
            [26, 27]],
           [[28, 29],
            [30, 31]]])
    >>> np.einsum('...ij,...j', a, b)
    array([[   9.,   29.],
           [  58.,   82.],
           [ 123.,  151.],
           [ 204.,  236.],
           [ 301.,  337.],
           [ 414.,  454.],
           [ 543.,  587.],
           [ 688.,  736.]])

numpy.dot can be used to find the dot product of each vector in a list with a corresponding vector in another list this is quite messy and slow compared with element-wise multiplication and summing along the last axis. Something like this (which requires a much larger array to be calculated but mostly ignored)

    >>> np.diag(np.dot(b,b.T))
    array([  82.,  104.,  130.,  160.,  194.,  232.,  274.,  320.])

Dot product using element-wise multiplication and summing

    >>> (b * b).sum(axis=-1)
    array([  82.,  104.,  130.,  160.,  194.,  232.,  274.,  320.])

Using einsum could be achieved with

    >>> np.einsum('...j,...j', b, b)
    array([  82.,  104.,  130.,  160.,  194.,  232.,  274.,  320.])




