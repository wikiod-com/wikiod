---
title: "Math behind 2D convolution with advanced examples in TF"
slug: "math-behind-2d-convolution-with-advanced-examples-in-tf"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

2D convolution is computed in a similar way one would calculate [1D convolution](https://www.wikiod.com/tensorflow/using-1d-convolution#Math behind 1D convolution with advanced examples in TF): you slide your kernel over the input, calculate the element-wise multiplications and sum them up. But instead of your kernel/input being an array, here they are matrices.

## No padding, strides=1
This is the most basic example, with the easiest calculations. Let's assume your `input` and `kernel` are:
[![enter image description here][1]][1]

When you your kernel you will receive the following output: [![enter image description here][2]][2], which is calculated in the following way:

 - 14 = 4 * 1 + 3 * 0 + 1 * 1 + 2 * 2 + 1 * 1 + 0 * 0 + 1 * 0 + 2 * 0 + 4 * 1
 - 6  = 3 * 1 + 1 * 0 + 0 * 1 + 1 * 2 + 0 * 1 + 1 * 0 + 2 * 0 + 4 * 0 + 1 * 1
 - 6  = 2 * 1 + 1 * 0 + 0 * 1 + 1 * 2 + 2 * 1 + 4 * 0 + 3 * 0 + 1 * 0 + 0 * 1
 - 12 = 1 * 1 + 0 * 0 + 1 * 1 + 2 * 2 + 4 * 1 + 1 * 0 + 1 * 0 + 0 * 0 + 2 * 1

TF's [conv2d][3] function calculates convolutions in batches and uses a slightly different format. For an input it is `[batch, in_height, in_width, in_channels]` for the kernel it is `[filter_height, filter_width, in_channels, out_channels]`. So we need to provide the data in the correct format:

    import tensorflow as tf
    k = tf.constant([
        [1, 0, 1],
        [2, 1, 0],
        [0, 0, 1]
    ], dtype=tf.float32, name='k')
    i = tf.constant([
        [4, 3, 1, 0],
        [2, 1, 0, 1],
        [1, 2, 4, 1],
        [3, 1, 0, 2]
    ], dtype=tf.float32, name='i')
    kernel = tf.reshape(k, [3, 3, 1, 1], name='kernel')
    image  = tf.reshape(i, [1, 4, 4, 1], name='image')

Afterwards the convolution is computed with:

    res = tf.squeeze(tf.nn.conv2d(image, kernel, [1, 1, 1, 1], "VALID"))
    # VALID means no padding
    with tf.Session() as sess:
       print sess.run(res)

And will be equivalent to the one we calculated by hand. 


  [1]: https://i.stack.imgur.com/yTCl8.png
  [2]: https://i.stack.imgur.com/TPhBi.png
  [3]: https://www.tensorflow.org/api_docs/python/tf/nn/conv2d

## Some padding, strides=1
Padding is just a fancy name of telling: surround your input matrix with some constant. In most of the cases the constant is zero and this is why people call it zero padding. So if you want to use a padding of 1 in our original input (check the first example with `padding=0, strides=1`), the matrix will look like this:

[![enter image description here][1]][1]

To calculate the values of the convolution you do the same sliding. Notice that in our case many values in the middle do not need to be recalculated (they will be the same as in previous example. I also will not show all the calculations here, because the idea is straight-forward. The result is:

[![enter image description here][2]][2]

where 
 - 5 = 0 * 1 + 0 * 0 + 0 * 1 + 0 * 2 + 4 * 1 + 3 * 0 + 0 * 0 + 0 * 1 + 1 * 1
 - ...
 - 6 = 4 * 1 + 1 * 0 + 0 * 1 + 0 * 2 + 2 * 1 + 0 * 0 + 0 * 0 + 0 * 0 + 0 * 1

TF does not support an arbitrary padding in [conv2d][3] function, so if you need some padding that is not supported, use [tf.pad()][4]. Luckily for our input the padding 'SAME' will be equal to padding = 1. So we need to change almost nothing in our previous example:

    res = tf.squeeze(tf.nn.conv2d(image, kernel, [1, 1, 1, 1], "SAME"))
    # 'SAME' makes sure that our output has the same size as input and 
    # uses appropriate padding. In our case it is 1.
    with tf.Session() as sess:
       print sess.run(res)

 You can verify that the answer will be the same as calculated by hand.


  [1]: https://i.stack.imgur.com/tPJk4.png
  [2]: https://i.stack.imgur.com/zQ6s9.png
  [3]: https://www.tensorflow.org/api_docs/python/tf/nn/conv2d
  [4]: https://www.tensorflow.org/api_docs/python/tf/pad

## Padding and strides (the most general case)
Now we will apply a strided convolution to our previously described padded example and calculate the convolution where `p = 1, s = 2`

[![enter image description here][1]][1]

Previously when we used `strides = 1`, our slided window moved by 1 position, with `strides = s` it moves by `s` positions (you need to calculate `s^2` elements less. But in our case we can take a shortcut and do not perform any computations at all. Because we already computed the values for `s = 1`, in our case we can just grab each second element. 

So if the solution is case of `s = 1` was 

[![enter image description here][2]][2]

in case of `s = 2` it will be:

[![enter image description here][3]][3]

Check the positions of values 14, 2, 12, 6 in the previous matrix. The only change we need to perform in our code is to change the strides from 1 to 2 for width and height dimension (2-nd, 3-rd).

    res = tf.squeeze(tf.nn.conv2d(image, kernel, [1, 2, 2, 1], "SAME"))
    with tf.Session() as sess:
       print sess.run(res)

By the way, there is nothing that stops us from using different strides for different dimensions.


  [1]: https://i.stack.imgur.com/68ZsM.png
  [2]: https://i.stack.imgur.com/rRse4.png
  [3]: https://i.stack.imgur.com/0Ydz6.png

