---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Declaring and Initializing Variable Tensors
Variable tensors are used when the values require updating within a session. It is the type of tensor that would be used for the weights matrix when creating neural networks, since these values will be updated as the model is being trained.

Declaring a variable tensor can be done using the `tf.Variable()` or `tf.get_variable()`function. It is recommended to use `tf.get_variable`, as it offers more flexibility eg:

```python
# Declare a 2 by 3 tensor populated by ones
a = tf.Variable(tf.ones([2,3], dtype=tf.float32))
a = tf.get_variable('a', shape=[2, 3], initializer=tf.constant_initializer(1))
```

Something to note is that declaring a variable tensor does not automatically initialize the values. The values need to be intialized explicitly when starting a session using one of the following: 


- `tf.global_variables_initializer().run()`
- `session.run(tf.global_variables_initializer())`


The following example shows the full process of declaring and initializing a variable tensor. 

```python
# Build a graph
graph = tf.Graph()
with graph.as_default():
    a = tf.get_variable('a', shape=[2,3], initializer=tf.constant_initializer(1), dtype=tf.float32))     # Create a variable tensor

# Create a session, and run the graph
with tf.Session(graph=graph) as session:
    tf.global_variables_initializer().run()  # Initialize values of all variable tensors
    output_a = session.run(a)            # Return the value of the variable tensor
    print(output_a)                      # Print this value
```

Which prints out the following:

    [[ 1.  1.  1.]
     [ 1.  1.  1.]]

## Fetch the value of a TensorFlow variable or a Tensor
Sometimes we need to fetch and print the value of a TensorFlow variable to guarantee our program is correct.

For example, if we have the following program:

    import tensorflow as tf
    import numpy as np
    a = tf.Variable(tf.random_normal([2,3])) # declare a tensorflow variable
    b = tf.random_normal([2,2]) #declare a tensorflow tensor
    init = tf.initialize_all_variables()

if we want to get the value of a or b, the following procedures can be used:

    with tf.Session() as sess:
        sess.run(init)
        a_value = sess.run(a)
        b_value = sess.run(b)
        print a_value
        print b_value

or

    with tf.Session() as sess:
        sess.run(init)
        a_value = a.eval()
        b_value = b.eval()
        print a_value
        print b_value

