---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

N-dimensional arrays or `ndarrays` are numpy's core object used for storing items of the same data type. They provide an efficient data structure that is superior to ordinary Python's arrays.

Whenever possible express operations on data in terms of arrays and vector operations.  Vector operations execute much faster than equivalent for loops

## Array Access
Slice syntax is `i:j:k` where `i` is the starting index (inclusive), `j` is the stopping index (exclusive) and `k` is the step size. Like other python data structures, the first element has an index of 0:

    x = np.arange(10)
    x[0]
    # Out: 0

    x[0:4]
    # Out: array([0, 1, 2, 3])

    x[0:4:2]
    # Out:array([0, 2])

Negative values count in from the end of the array.  `-1` therefore accesses the last element in an array:

    x[-1]
    # Out: 9
    x[-1:0:-1]
    # Out: array([9, 8, 7, 6, 5, 4, 3, 2, 1])

Multi-dimensional arrays can be accessed by specifying each dimension separated by commas.  All previous rules apply.

    x = np.arange(16).reshape((4,4))
    x
    # Out: 
    #     array([[ 0,  1,  2,  3],
    #            [ 4,  5,  6,  7],
    #            [ 8,  9, 10, 11],
    #            [12, 13, 14, 15]])

    x[1,1]
    # Out: 5

    x[0:3,0]
    # Out: array([0, 4, 8])

    x[0:3, 0:3]
    # Out: 
    #     array([[ 0,  1,  2],
    #            [ 4,  5,  6],
    #            [ 8,  9, 10]])

    x[0:3:2, 0:3:2]
    # Out: 
    #     array([[ 0,  2],
    #            [ 8, 10]])

## Create an Array
**Empty array**

    np.empty((2,3))

Note that in this case, the values in this array are not set. This way of creating an array is therefore only useful if the array is filled later in the code.

**From a list**

    np.array([0,1,2,3])
    # Out: array([0, 1, 2, 3]) 

**Create a range**

    np.arange(4)
    # Out: array([0, 1, 2, 3])

**Create Zeros**

    np.zeros((3,2))
    # Out:
    # array([[ 0.,  0.],
    #        [ 0.,  0.],
    #        [ 0.,  0.]])

**Create Ones**

    np.ones((3,2))
    # Out:
    # array([[ 1.,  1.],
    #        [ 1.,  1.],
    #        [ 1.,  1.]])

**Create linear-spaced array items**

    np.linspace(0,1,21)
    # Out:
    # array([ 0.  ,  0.05,  0.1 ,  0.15,  0.2 ,  0.25,  0.3 ,  0.35,  0.4 ,
    #         0.45,  0.5 ,  0.55,  0.6 ,  0.65,  0.7 ,  0.75,  0.8 ,  0.85,
    #         0.9 ,  0.95,  1.  ])

**Create log-spaced array items**

    np.logspace(-2,2,5)
    # Out:
    # array([  1.00000000e-02,   1.00000000e-01,   1.00000000e+00,
    #          1.00000000e+01,   1.00000000e+02])

**Create array from a given function**

    np.fromfunction(lambda i: i**2, (5,))
    # Out:
    # array([  0.,   1.,   4.,   9.,  16.])
    np.fromfunction(lambda i,j: i**2, (3,3))
    # Out:
    # array([[ 0.,  0.,  0.],
    #        [ 1.,  1.,  1.],
    #        [ 4.,  4.,  4.]])



## Array operators
    x = np.arange(4)
    x
    #Out:array([0, 1, 2, 3])

scalar addition is element wise

    x+10
    #Out: array([10, 11, 12, 13])

scalar multiplication is element wise

    x*2
    #Out: array([0, 2, 4, 6])

array addition is element wise

    x+x
    #Out: array([0, 2, 4, 6]) 

array multiplication is element wise

    x*x
    #Out: array([0, 1, 4, 9])

dot product (or more generally matrix multiplication) is done with a function 

    x.dot(x)
    #Out: 14

<!-- if version Python [gte 3.5] -->

In Python 3.5, the `@` operator was added as an infix operator for matrix multiplication
    
    x = np.diag(np.arange(4))
    print(x)
    '''
       Out: array([[0, 0, 0, 0],
       [0, 1, 0, 0],
       [0, 0, 2, 0],
       [0, 0, 0, 3]])
    '''
    print(x@x)
    print(x)
    '''
       Out: array([[0, 0, 0, 0],
       [0, 1, 0, 0],
       [0, 0, 4, 0],
       [0, 0, 0, 9]])
    '''
**Append**. Returns copy with values appended. NOT in-place. 

    #np.append(array, values_to_append, axis=None)
    x = np.array([0,1,2,3,4])
    np.append(x, [5,6,7,8,9])
    # Out: array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    x
    # Out: array([0, 1, 2, 3, 4])
    y = np.append(x, [5,6,7,8,9])
    y
    # Out: array([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])

**hstack**. Horizontal stack. (column stack)<br>
**vstack**. Vertical stack. (row stack)

    # np.hstack(tup), np.vstack(tup)
    x = np.array([0,0,0])
    y = np.array([1,1,1])
    z = np.array([2,2,2])
    np.hstack(x,y,z)
    # Out: array([0, 0, 0, 1, 1, 1, 2, 2, 2])
    np.vstack(x,y,z)
    # Out: array([[0, 0, 0],
    #             [1, 1, 1],
    #             [2, 2, 2]])
    

<!-- end version if -->

## Boolean indexing
    arr = np.arange(7)
    print(arr)
    # Out: array([0, 1, 2, 3, 4, 5, 6])

Comparison with a scalar returns a boolean array:

    arr > 4
    # Out: array([False, False, False, False, False,  True,  True], dtype=bool)

This array can be used in indexing to select only the numbers greater than 4:

    arr[arr>4]
    # Out: array([5, 6])

Boolean indexing can be used between different arrays (e.g. related parallel arrays):

    # Two related arrays of same length, i.e. parallel arrays
    idxs = np.arange(10)
    sqrs = idxs**2

    # Retrieve elements from one array using a condition on the other
    my_sqrs = sqrs[idxs % 2 == 0]
    print(my_sqrs)
    # Out: array([0, 4, 16, 36, 64])

## Broadcasting array operations
Arithmetic operations are performed elementwise on Numpy arrays. For arrays of identical shape, this means that the operation is executed between elements at corresponding indices.

    # Create two arrays of the same size
    a = np.arange(6).reshape(2, 3)
    b = np.ones(6).reshape(2, 3)

    a
    # array([0, 1, 2],
    #       [3, 4, 5])
    b
    # array([1, 1, 1],
    #       [1, 1, 1])
    
    # a + b: a and b are added elementwise
    a + b
    # array([1, 2, 3],
    #       [4, 5, 6])

Arithmetic operations may also be executed on arrays of different shapes by means of Numpy _broadcasting_. In general, one array is "broadcast" over the other so that elementwise operations are performed on sub-arrays of congruent shape.

    # Create arrays of shapes (1, 5) and (13, 1) respectively
    a = np.arange(5).reshape(1, 5)
    a
    # array([[0, 1, 2, 3, 4]])
    b = np.arange(4).reshape(4, 1)
    b
    # array([0],
    #       [1],
    #       [2],
    #       [3])
    
    # When multiplying a * b, slices with the same dimensions are multiplied
    # elementwise. In the case of a * b, the one and only row of a is multiplied
    # with each scalar down the one and only column of b.
    a*b
    # array([[ 0,  0,  0,  0,  0],
    #        [ 0,  1,  2,  3,  4],
    #        [ 0,  2,  4,  6,  8],
    #        [ 0,  3,  6,  9, 12]])

To illustrate this further, consider the multiplication of 2D and 3D arrays with congruent sub-dimensions.

    # Create arrays of shapes (2, 2, 3) and (2, 3) respectively
    a = np.arange(12).reshape(2, 2, 3)
    a
    # array([[[ 0  1  2]
    #         [ 3  4  5]]
    #
    #        [[ 6  7  8]
    #         [ 9 10 11]]])
    b = np.arange(6).reshape(2, 3)
    # array([[0, 1, 2],
    #        [3, 4, 5]])
    
    # Executing a*b broadcasts b to each (2, 3) slice of a,
    # multiplying elementwise.
    a*b
    # array([[[ 0,  1,  4],
    #         [ 9, 16, 25]],
    #
    #        [[ 0,  7, 16],
    #         [27, 40, 55]]])
    
    # Executing b*a gives the same result, i.e. the smaller
    # array is broadcast over the other.

# When is array broadcasting applied?

Broadcasting takes place when two arrays have _compatible_ shapes.

Shapes are compared component-wise starting from the trailing ones. Two dimensions are compatible if either they're the same or one of them is `1`. If one shape has higher dimension than the other, the exceeding components are not compared.

Some examples of compatible shapes:

    (7, 5, 3)    # compatible because dimensions are the same
    (7, 5, 3)

    
    (7, 5, 3)    # compatible because second dimension is 1  
    (7, 1, 3)

    (7, 5, 3, 5) # compatible because exceeding dimensions are not compared
          (3, 5)

    (3, 4, 5)    # incompatible 
       (5, 5) 

    (3, 4, 5)    # compatible 
       (1, 5) 

Here's the official documentation on [array broadcasting](https://docs.scipy.org/doc/numpy-1.12.0/user/basics.broadcasting.html).

## Transposing an array
    arr = np.arange(10).reshape(2, 5)

Using `.transpose` method:

    arr.transpose()
    # Out:  
    #      array([[0, 5],
    #            [1, 6],
    #            [2, 7],
    #            [3, 8],
    #            [4, 9]])

`.T` method:

    arr.T
    # Out: 
    #     array([[0, 5],
    #            [1, 6],
    #            [2, 7],
    #            [3, 8],
    #            [4, 9]])

Or `np.transpose`:

    np.transpose(arr)
    # Out:
    #     array([[0, 5],
    #            [1, 6],
    #            [2, 7],
    #            [3, 8],
    #            [4, 9]])

In the case of a 2-dimensional array, this is equivalent to a standard matrix transpose (as depicted above). In the n-dimensional case, you may specify a permutation of the array axes. By default, this reverses `array.shape`:

    a = np.arange(12).reshape((3,2,2))
    a.transpose() # equivalent to a.transpose(2,1,0)
    # Out:
    #   array([[[ 0,  4,  8],
    #           [ 2,  6, 10]],
    #
    #          [[ 1,  5,  9],
    #           [ 3,  7, 11]]])


But any permutation of the axis indices is possible:

    a.transpose(2,0,1)
    # Out:
    #    array([[[ 0,  2],
    #            [ 4,  6],
    #            [ 8, 10]],
    #    
    #           [[ 1,  3],
    #            [ 5,  7],
    #            [ 9, 11]]])
    
    a = np.arange(24).reshape((2,3,4))  # shape (2,3,4)
    a.transpose(2,0,1).shape
    # Out:
    #    (4, 2, 3)




## Reshaping an array
The `numpy.reshape` (same as `numpy.ndarray.reshape`) method returns an array of the same total size, but in a new shape:

    print(np.arange(10).reshape((2, 5)))   
    # [[0 1 2 3 4]
    #  [5 6 7 8 9]]

It returns a new array, and doesn't operate in place:

    a = np.arange(12)
    a.reshape((3, 4))
    print(a)
    # [ 0  1  2  3  4  5  6  7  8  9 10 11]

However, it *is* possible to overwrite the `shape` attribute of an `ndarray`:

    a = np.arange(12)
    a.shape = (3, 4)
    print(a)
    # [[ 0  1  2  3]
    #  [ 4  5  6  7]
    #  [ 8  9 10 11]]

This behavior might be surprising at first, but `ndarray`s are stored in contiguous blocks of memory, and their `shape` only specifies how this stream of data should be interpreted as a multidimensional object.

Up to one axis in the `shape` tuple can have a value of `-1`. `numpy` will then infer the length of this axis for you:

    a = np.arange(12)
    print(a.reshape((3, -1)))
    # [[ 0  1  2  3]
    #  [ 4  5  6  7]
    #  [ 8  9 10 11]]

Or:

    a = np.arange(12)
    print(a.reshape((3, 2, -1)))
    
    # [[[ 0  1]
    #   [ 2  3]]

    #  [[ 4  5]
    #   [ 6  7]]

    #  [[ 8  9]
    #   [10 11]]]

Multiple unspecified dimensions, e.g. `a.reshape((3, -1, -1))` are not allowed and will throw a `ValueError`.


## Populate an array with the contents of a CSV file
    filePath = "file.csv"
    data = np.genfromtxt(filePath)

Many options are supported, see [official documentation][1] for full list:

    data = np.genfromtxt(filePath, dtype='float', delimiter=';', skip_header=1, usecols=(0,1,3) )


  [1]: http://docs.scipy.org/doc/numpy/reference/generated/numpy.genfromtxt.html

## Numpy n-dimensional array: the ndarray
The core data structure in numpy is the `ndarray` (short for _n_-dimensional array). `ndarray`s are
 
 - homogeneous (i.e. they contain items of the same data-type)
 - contain items of fixed sizes (given by a _shape_, a tuple of _n_ positive integers that specify the sizes of each dimension)
 
 One-dimensional array:
 
    x = np.arange(15)
    # array([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14])
    x.shape
    # (15,)
 
 Two-dimensional array:
 
    x = np.asarray([[0, 1, 2, 3, 4], [5, 6, 7, 8, 9], [10, 11, 12, 13, 14]])
    x
    # array([[ 0, 1, 2, 3, 4],
    # [ 5, 6, 7, 8, 9],
    # [10, 11, 12, 13, 14]])
    x.shape
    # (3, 5)
 
 Three-dimensional:
 
    np.arange(12).reshape([2,3,2])
 
 To initialize an array without specifying its contents use:
 
    x = np.empty([2, 2])
    # array([[ 0., 0.],
    # [ 0., 0.]])
 
 **Datatype guessing and automatic casting**
 
 The data-type is set to float by default
 
    x = np.empty([2, 2])
    # array([[ 0., 0.],
    # [ 0., 0.]])
 
    x.dtype
    # dtype('float64')
 
 If some data is provided, numpy will guess the data-type:
 
    x = np.asarray([[1, 2], [3, 4]])
    x.dtype
    # dtype('int32')
 
 Note that when doing assignments numpy will attempt to automatically cast values to suit the `ndarray`'s datatype
 
    x[1, 1] = 1.5 # assign a float value
    x[1, 1]
    # 1 
    # value has been casted to int
    x[1, 1] = 'z' # value cannot be casted, resulting in a ValueError
 
 **Array broadcasting**
 
 See also https://www.wikiod.com/numpy/arrays#Broadcasting array operations
 
    x = np.asarray([[1, 2], [3, 4]])
    # array([[1, 2],
             [3, 4]])
    y = np.asarray([[5, 6]])
    # array([[5, 6]])
 
 In matrix terminology, we would have a 2x2 matrix and a 1x2 row vector. Still we're able to do a sum
 
    # x + y
    array([[ 6, 8],
           [ 8, 10]])
 
 This is because the array `y` is "_stretched_" to:
 
    array([[5, 6],
           [5, 6]])
 
 to suit the shape of `x`.
 
 **Resources:**
 
 - Introduction to the ndarray from the official documentation: [The N-dimensional array (ndarray)](https://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html)
 - Class reference: [ndarray](https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.html).
 

