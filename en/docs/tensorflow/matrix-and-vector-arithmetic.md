---
title: "Matrix and Vector Arithmetic"
slug: "matrix-and-vector-arithmetic"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Dot Product
The dot product between two tensors can be performed using: 

```
tf.matmul(a, b)
```

A full example is given below: 

```
# Build a graph
graph = tf.Graph()
with graph.as_default():
    # A 2x3 matrix
    a = tf.constant(np.array([[1, 2, 3],
                              [2, 4, 6]]),
                            dtype=tf.float32)
    # A 3x2 matrix
    b = tf.constant(np.array([[1, 10],
                              [2, 20],
                              [3, 30]]),
                    dtype=tf.float32)

    # Perform dot product
    c = tf.matmul(a, b)

# Run a Session
with tf.Session(graph=graph) as session:
    output = session.run(c)
    print(output)
```

prints out 

    [[  14.  140.]
     [  28.  280.]]



## Elementwise Multiplication
To perform elementwise multiplication on tensors, you can use either of the following:

- `a*b`
- `tf.multiply(a, b)`

Here is a full example of elementwise multiplication using both methods. 

```python
import tensorflow as tf
import numpy as np

# Build a graph
graph = tf.Graph()
with graph.as_default():
    # A 2x3 matrix
    a = tf.constant(np.array([[ 1, 2, 3],
                              [10,20,30]]),
                    dtype=tf.float32)
    # Another 2x3 matrix
    b = tf.constant(np.array([[2, 2, 2],
                              [3, 3, 3]]),
                    dtype=tf.float32)

    # Elementwise multiplication
    c =  a * b
    d = tf.multiply(a, b)

# Run a Session
with tf.Session(graph=graph) as session:
    (output_c, output_d) = session.run([c, d])
    print("output_c")
    print(output_c)
    print("\noutput_d")
    print(output_d)
```

Prints out the following:

    output_c
    [[  2.   4.   6.]
     [ 30.  60.  90.]]
    
    output_d
    [[  2.   4.   6.]
     [ 30.  60.  90.]]


## Scalar Times a Tensor
In the following example a 2 by 3 tensor is multiplied by a scalar value (2). 

```
# Build a graph
graph = tf.Graph()
with graph.as_default():
    # A 2x3 matrix
    a = tf.constant(np.array([[ 1, 2, 3],
                              [10,20,30]]),
                    dtype=tf.float32)
                    
    # Scalar times Matrix
    c =  2 * a

# Run a Session
with tf.Session(graph=graph) as session:
    output = session.run(c)
    print(output)
```

This prints out

    [[  2.   4.   6.]
     [ 20.  40.  60.]]


