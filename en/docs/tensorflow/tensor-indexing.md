---
title: "Tensor indexing"
slug: "tensor-indexing"
draft: false
images: []
weight: 9777
type: docs
toc: true
---

Various examples showing how Tensorflow supports indexing into tensors, highlighting differences and similarities to numpy-like indexing where possible.

## Extract a slice from a tensor
Refer to the [`tf.slice(input, begin, size)`][1] documentation for detailed information.

Arguments:

- `input`: Tensor
- `begin`: starting location for each dimension of `input`
- `size`: number of elements for each dimension of `input`, using `-1` includes all remaining elements

---
Numpy-like slicing:

<!-- language: lang-py -->
    # x has shape [2, 3, 2]
    x = tf.constant([[[1., 2.], [3., 4. ], [5. , 6. ]],
                     [[7., 8.], [9., 10.], [11., 12.]]])

    # Extracts x[0, 1:2, :] == [[[ 3.,  4.]]]
    res = tf.slice(x, [0, 1, 0], [1, 1, -1])

---
Using negative indexing, to retrieve the last element in the third dimension:

<!-- language: lang-py -->
    # Extracts x[0, :, -1:] == [[[2.], [4.], [6.]]]
    last_indice = x.get_shape().as_list()[2] - 1
    res = tf.slice(x, [0, 1, last_indice], [1, -1, -1])
    


[1]: https://www.tensorflow.org/api_docs/python/tf/slice

## How to use tf.gather_nd
[`tf.gather_nd`][1] is an extension of [`tf.gather`][2] in the sense that it allows you to not only access the 1st dimension of a tensor, but potentially all of them.

Arguments:
  - `params`: a Tensor of rank `P` representing the tensor we want to index into
  - `indices`: a Tensor of rank `Q` representing the indices into `params` we want to access

The output of the function depends on the shape of `indices`. If the innermost dimension of `indices` has length `P`, we are collecting single elements from `params`. If it is less than `P`, we are collecting slices, just like with `tf.gather` but without the restriction that we can only access the 1st dimension.

-----------------
**Collecting elements from a tensor of rank 2**

To access the element at `(1, 2)` in a matrix, we can use:

    # data is [[0, 1, 2, 3, 4, 5],
    #          [6, 7, 8, 9, 10, 11],
    #          [12 13 14 15 16 17],
    #          [18 19 20 21 22 23],
    #          [24, 25, 26, 27, 28, 29]]
    data = np.reshape(np.arange(30), [5, 6])
    x = tf.constant(data)
    result = tf.gather_nd(x, [1, 2])

where `result` will just be `8` as expected. Note how this is different from `tf.gather`: the same indices passed to `tf.gather(x, [1, 2])` would have given as the 2nd and 3rd *row* from `data`.

If you want to retrieve more than one element at the same time, just pass a list of index pairs:

    result = tf.gather_nd(x, [[1, 2], [4, 3], [2, 5]])

which will return `[ 8 27 17]`

-----------------
**Collecting rows from a tensor of rank 2**

If in the above example you want to collect rows (i.e. slices) instead of elements, adjust the `indices` parameter as follows:

    data = np.reshape(np.arange(30), [5, 6])
    x = tf.constant(data)
    result = tf.gather_nd(x, [[1], [3]])

This will give you the 2nd and 4th row of `data`, i.e.

    [[ 6  7  8  9 10 11]
     [18 19 20 21 22 23]]

-----------------
**Collecting elements from a tensor of rank 3**

The concept of how to access rank-2 tensors directly translates to higher dimensional tensors. So, to access elements in a rank-3 tensor, the innermost dimension of `indices` must have length 3.

    # data is [[[ 0  1]
    #          [ 2  3]
    #          [ 4  5]]
    #
    #         [[ 6  7]
    #          [ 8  9]
    #          [10 11]]]
    data = np.reshape(np.arange(12), [2, 3, 2])
    x = tf.constant(data)
    result = tf.gather_nd(x, [[0, 0, 0], [1, 2, 1]])

`result` will now look like this: `[ 0 11]`

-----------------
**Collecting batched rows from a tensor of rank 3**

Let's think of a rank-3 tensor as a batch of matrices shaped `(batch_size, m, n)`. If you want to collect the first and second row for every element in the batch, you could use this:

    # data is [[[ 0  1]
    #          [ 2  3]
    #          [ 4  5]]
    #
    #         [[ 6  7]
    #          [ 8  9]
    #          [10 11]]]
    data = np.reshape(np.arange(12), [2, 3, 2])
    x = tf.constant(data)
    result = tf.gather_nd(x, [[[0, 0], [0, 1]], [[1, 0], [1, 1]]])

which will result in this:

    [[[0 1]
      [2 3]]
    
     [[6 7]
      [8 9]]]

Note how the shape of `indices` influences the shape of the output tensor. If we would have used a rank-2 tensor for the `indices` argument:

    result = tf.gather_nd(x, [[0, 0], [0, 1], [1, 0], [1, 1]])

the output would have been

    [[0 1]
     [2 3]
     [6 7]
     [8 9]]


  [1]: https://www.tensorflow.org/api_docs/python/tf/gather_nd
  [2]: https://www.tensorflow.org/api_docs/python/tf/gather

## Extract non-contiguous slices from the first dimension of a tensor
Generally `tf.gather` gives you access to elements in the first dimension of a tensor (e.g. rows 1, 3 and 7 in a 2-dimensional Tensor). If you need access to any other dimension than the first one, or if you don't need the whole slice, but e.g. only the 5th entry in the 1st, 3rd and 7th row, you are better off using `tf.gather_nd` (see upcoming example for this).

`tf.gather` arguments:

 - `params`: A tensor you want to extract values from.
 - `indices`: A tensor specifying the indices pointing into `params`

Refer to the [tf.gather(params, indices)][1] documentation for detailed information.

----------
We want to extract the 1st and 4th row in a 2-dimensional tensor.

    # data is [[0, 1, 2, 3, 4, 5],
    #          [6, 7, 8, 9, 10, 11],
    #          ...
    #          [24, 25, 26, 27, 28, 29]]
    data = np.reshape(np.arange(30), [5, 6])
    params = tf.constant(data)
    indices = tf.constant([0, 3])
    selected = tf.gather(params, indices)

`selected` has shape `[2, 6]` and printing its value gives

    [[ 0  1  2  3  4  5]
     [18 19 20 21 22 23]]


----------
`indices` can also just be a scalar (but cannot contain negative indices). E.g. in the above example:

    tf.gather(params, tf.constant(3))

would print

    [18 19 20 21 22 23]

----------
Note that `indices` can have any shape, but the elements stored in `indices` always only refer to the *first* dimension of `params`. E.g. if you want to retrieve both the 1st and 3rd row *and* the 2nd and 4th row at the same time, you can do this:

    indices = tf.constant([[0, 2], [1, 3]])
    selected = tf.gather(params, indices)

Now `selected` will have shape `[2, 2, 6]` and its content reads:

    [[[ 0  1  2  3  4  5]
      [12 13 14 15 16 17]]
    
     [[ 6  7  8  9 10 11]
      [18 19 20 21 22 23]]]


----------
You can use `tf.gather` to compute a permutation. E.g. the following reverses all rows of `params`:

    indices = tf.constant(list(range(4, -1, -1)))
    selected = tf.gather(params, indices)

`selected` is now

    [[24 25 26 27 28 29]
     [18 19 20 21 22 23]
     [12 13 14 15 16 17]
     [ 6  7  8  9 10 11]
     [ 0  1  2  3  4  5]]

----------
If you need access to any other than the first dimension, you could work around that using `tf.transpose`: E.g. to gather columns instead of rows in our example, you could do this:

    indices = tf.constant([0, 2])
    selected = tf.gather(tf.transpose(params, [1, 0]), indices)
    selected_t = tf.transpose(selected, [1, 0]) 

`selected_t` is of shape `[5, 2]` and reads:

    [[ 0  2]
     [ 6  8]
     [12 14]
     [18 20]
     [24 26]]

However, `tf.transpose` is rather expensive, so it might be better to use `tf.gather_nd` for this use case.

  [1]: https://www.tensorflow.org/versions/r0.12/api_docs/python/array_ops/slicing_and_joining#gather

## Numpy-like indexing using tensors
This example is based on this post: http://stackoverflow.com/questions/33736795/tensorflow-numpy-like-tensor-indexing.

In Numpy you can use arrays to index into an array. E.g. in order to select the elements at `(1, 2)` and `(3, 2)` in a 2-dimensional array, you can do this:

    # data is [[0, 1, 2, 3, 4, 5],
    #          [6, 7, 8, 9, 10, 11],
    #          [12 13 14 15 16 17],
    #          [18 19 20 21 22 23],
    #          [24, 25, 26, 27, 28, 29]]
    data = np.reshape(np.arange(30), [5, 6])
    a = [1, 3]
    b = [2, 2]
    selected = data[a, b]
    print(selected)

This will print:

    [ 8 20]

To get the same behaviour in Tensorflow, you can use [`tf.gather_nd`][1], which is an extension of `tf.gather`. The above example can be written like this:

    x = tf.constant(data)
    idx1 = tf.constant(a)
    idx2 = tf.constant(b)
    result = tf.gather_nd(x, tf.stack((idx1, idx2), -1))
            
    with tf.Session() as sess:
        print(sess.run(result))

This will print:

    [ 8 20]

`tf.stack` is the equivalent of `np.asarray` and in this case stacks the two index vectors along the last dimension (which in this case is the 1st) to produce:

    [[1 2]
     [3 2]]


  [1]: https://www.tensorflow.org/api_docs/python/tf/gather_nd

