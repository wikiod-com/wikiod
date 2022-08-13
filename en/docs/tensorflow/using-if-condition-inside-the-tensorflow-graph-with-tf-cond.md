---
title: "Using if condition inside the TensorFlow graph with tf.cond"
slug: "using-if-condition-inside-the-tensorflow-graph-with-tfcond"
draft: false
images: []
weight: 9563
type: docs
toc: true
---

## Parameters
Parameter | Details
---       | ---
pred      | a TensorFlow tensor of type `bool`
fn1       | a callable function, **with no argument**
fn2       | a callable function, **with no argument**
name| (optional) name for the operation

- `pred` cannot be just `True` or `False`, it needs to be a Tensor
- The function `fn1` and `fn2` should return the same number of outputs, with the same types.

## Basic example
<!-- language: lang-py -->
    x = tf.constant(1.)
    bool = tf.constant(True)

    res = tf.cond(bool, lambda: tf.add(x, 1.), lambda: tf.add(x, 10.))
    # sess.run(res) will give you 2.

## When f1 and f2 return multiple tensors
The two functions `fn1` and `fn2` can return multiple tensors, but they have to return the exact same number and types of outputs.

<!-- language: lang-py -->

    x = tf.constant(1.)
    bool = tf.constant(True)

    def fn1():
        return tf.add(x, 1.), x

    def fn2():
        return tf.add(x, 10.), x
    
    res1, res2 = tf.cond(bool, fn1, fn2)
    # tf.cond returns a list of two tensors
    # sess.run([res1, res2]) will return [2., 1.]

## define and use functions f1 and f2 with parameters
You can pass parameters to the functions in tf.cond() using **lambda** and the code is as bellow.

<!-- language: lang-py -->
    x = tf.placeholder(tf.float32)
    y = tf.placeholder(tf.float32)
    z = tf.placeholder(tf.float32)
    
    def fn1(a, b):
      return tf.mul(a, b)
    
    def fn2(a, b):
      return tf.add(a, b)
    
    pred = tf.placeholder(tf.bool)
    result = tf.cond(pred, lambda: fn1(x, y), lambda: fn2(y, z))

Then you can call it as bellowing:

<!-- language: lang-py -->
    with tf.Session() as sess:
      print sess.run(result, feed_dict={x: 1, y: 2, z: 3, pred: True})
      # The result is 2.0
      print sess.run(result, feed_dict={x: 1, y: 2, z: 3, pred: False})
      # The result is 5.0

