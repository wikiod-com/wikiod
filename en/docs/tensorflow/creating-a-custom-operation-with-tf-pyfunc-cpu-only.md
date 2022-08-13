---
title: "Creating a custom operation with tf.py_func (CPU only)"
slug: "creating-a-custom-operation-with-tfpy_func-cpu-only"
draft: false
images: []
weight: 9738
type: docs
toc: true
---

## Parameters
Parameter | Details
---       | ---
func      | python function, which takes **numpy arrays** as its inputs and returns **numpy arrays** as its outputs
inp       | list of Tensors (inputs)
Tout      | list of tensorflow data types for the outputs of `func`

## Why to use tf.py_func
The `tf.py_func()` operator enables you to run arbitrary Python code in the middle of a TensorFlow graph. It is particularly convenient for wrapping custom NumPy operators for which no equivalent TensorFlow operator (yet) exists. Adding `tf.py_func()` is an alternative to using `sess.run()` calls inside the graph.

Another way of doing that is to cut the graph in two parts:

<!-- language: lang-py -->
    # Part 1 of the graph
    inputs = ...  # in the TF graph

    # Get the numpy array and apply func
    val = sess.run(inputs)  # get the value of inputs
    output_val = func(val)  # numpy array
    
    # Part 2 of the graph
    output = tf.placeholder(tf.float32, shape=...)
    train_op = ...

    # We feed the output_val to the tensor output
    sess.run(train_op, feed_dict={output: output_val})

---

With `tf.py_func` this is much easier:

<!-- language: lang-py -->

    # Part 1 of the graph
    inputs = ...

    # call to tf.py_func
    output = tf.py_func(func, [inputs], [tf.float32])[0]

    # Part 2 of the graph
    train_op = ...

    # Only one call to sess.run, no need of a intermediate placeholder
    sess.run(train_op)
    

## Basic example
The `tf.py_func(func, inp, Tout)` operator creates a TensorFlow operation that calls a Python function, `func` on a list of tensors `inp`.

See the [documentation][1] for `tf.py_func(func, inp, Tout)`.

**Warning**: The `tf.py_func()` operation will only run on CPU. If you are using distributed TensorFlow, the `tf.py_func()` operation must be placed on a CPU device **in the same process** as the client.

<!-- language: lang-py -->

    def func(x):
        return 2*x
    
    x = tf.constant(1.)
    res = tf.py_func(func, [x], [tf.float32])
    # res is a list of length 1


[1]: https://www.tensorflow.org/versions/master/api_docs/python/script_ops.html#py_func

