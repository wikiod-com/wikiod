---
title: "Using 1D convolution"
slug: "using-1d-convolution"
draft: false
images: []
weight: 8735
type: docs
toc: true
---

## Basic example
**Update:** TensorFlow now supports 1D convolution since version r0.11, using [`tf.nn.conv1d`](https://www.tensorflow.org/versions/r0.11/api_docs/python/nn.html#conv1d).



---

Consider a basic example with an input of length `10`, and dimension `16`. The batch size is `32`. We therefore have a placeholder with input shape `[batch_size, 10, 16]`.

 

<!-- language: lang-py -->
    batch_size = 32
    x = tf.placeholder(tf.float32, [batch_size, 10, 16])

We then create a filter with width 3, and we take `16` channels as input, and output also `16` channels.

<!-- language: lang-py -->
    filter = tf.zeros([3, 16, 16])  # these should be real values, not 0

---

Finally we apply `tf.nn.conv1d` with a stride and a padding:
- **stride**: integer `s`
- **padding**: this works like in 2D, you can choose between `SAME` and `VALID`. `SAME` will output the same input length, while `VALID` will not add zero padding.

For our example we take a stride of 2, and a valid padding.
<!-- language: lang-py -->
    output = tf.nn.conv1d(x, filter, stride=2, padding="VALID")

The output shape should be `[batch_size, 4, 16]`.  
With `padding="SAME"`, we would have had an output shape of `[batch_size, 5, 16]`.


---

For previous versions of TensorFlow, you can just use 2D convolutions while setting the height of the inputs and the filters to `1`.

## Math behind 1D convolution with advanced examples in TF
`To calculate 1D convolution by hand, you slide your kernel over the input, calculate the element-wise multiplications and sum them up.

# The easiest way is for padding=0, stride=1

So if your `input = [1, 0, 2, 3, 0, 1, 1]` and `kernel = [2, 1, 3]` the result of the convolution is `[8, 11, 7, 9, 4]`, which is calculated in the following way:

 - *8  = 1 * 2 + 0 * 1 + 2 * 3*
 - *11 = 0 * 2 + 2 * 1 + 3 * 3*
 - *7  = 2 * 2 + 3 * 1 + 0 * 3*
 - *9  = 3 * 2 + 0 * 1 + 1 * 3*
 - *4  = 0 * 2 + 1 * 1 + 1 * 3*

TF's [conv1d][1] function calculates convolutions in batches, so in order to do this in TF, we need to provide the data in the correct format (doc explains that input should be in `[batch, in_width, in_channels]`, it also explains how kernel should look like). So 

    import tensorflow as tf
    i = tf.constant([1, 0, 2, 3, 0, 1, 1], dtype=tf.float32, name='i')
    k = tf.constant([2, 1, 3], dtype=tf.float32, name='k')
    
    print i, '\n', k, '\n'
    
    data   = tf.reshape(i, [1, int(i.shape[0]), 1], name='data')
    kernel = tf.reshape(k, [int(k.shape[0]), 1, 1], name='kernel')
    
    print data, '\n', kernel, '\n'
    
    res = tf.squeeze(tf.nn.conv1d(data, kernel, 1, 'VALID'))
    with tf.Session() as sess:
        print sess.run(res)

which will give you the same answer we calculated previously: `[  8.  11.   7.   9.   4.]`

# Convolution with padding

Padding is just a fancy way to tell append and prepend your input with some value. In most of the cases this value is 0, and this is why most of the time people name it zero-padding. TF support 'VALID' and 'SAME' zero-padding, for an arbitrary padding you need to use [tf.pad()][2]. 'VALID' padding means no padding at all, where the same means that the output will have the same size of the input. Let's calculate the convolution with `padding=1` on the same example (notice that for our kernel this is 'SAME' padding). To do this we just append our array with 1 zero at the beginning/end: `input = [0, 1, 0, 2, 3, 0, 1, 1, 0]`.

Here you can notice that you do not need to recalculate everything: all the elements stay the same except of the first/last one which are:
 - *1 = 0 * 2 + 1 * 1 + 0 * 3*
 - *3 = 1 * 2 + 1 * 1 + 0 * 3*

So the result is `[1, 8, 11, 7, 9, 4, 3]` which is the same as calculated with TF:

    res = tf.squeeze(tf.nn.conv1d(data, kernel, 1, 'SAME'))
    with tf.Session() as sess:
        print sess.run(res)

# Convolution with strides

Strides allow you to skip elements while sliding. In all our previous examples we slided 1 element, now you can slide `s` elements at a time. Because we will use a previous example, there is a trick: sliding by `n` elements is equivalent to sliding by 1 element and selecting every n-th element.

So if we use our previous example with `padding=1` and change `stride` to 2, you just take the previous result `[1, 8, 11, 7, 9, 4, 3]` and leave each 2-nd element, which will result in `[1, 11, 9, 3]`. You can do this in TF in the following way:

    res = tf.squeeze(tf.nn.conv1d(data, kernel, 2, 'SAME'))
    with tf.Session() as sess:
        print sess.run(res)


  [1]: https://www.tensorflow.org/api_docs/python/tf/nn/conv1d
  [2]: https://www.tensorflow.org/api_docs/python/tf/pad

