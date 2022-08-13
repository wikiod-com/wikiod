---
title: "Getting started with tensorflow"
slug: "getting-started-with-tensorflow"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Example
<!-- language-all: lang-py -->
Tensorflow is more than just a deep learning framework.  It is a general computation framework to perform general mathematical operations in a parallel and distributed manner.  An example of such is described below.

# Linear Regression

A basic statistical example that is commonly utilized and is rather simple to compute is fitting a line to a dataset. The method to do so in tensorflow is described below in code and comments.

The main steps of the (TensorFlow) script are:

 1. Declare [placeholders][1] (`x_ph`, `y_ph`) and [variables][2] (`W`, `b`)
 2. Define the initialization operator (`init`)
 3. Declare operations on the placeholders and variables (`y_pred`, `loss`, `train_op`)
 4. Create a session (`sess`)
 5. Run the initialization operator (`sess.run(init)`)
 6. Run some graph operations (e.g. `sess.run([train_op, loss], feed_dict={x_ph: x, y_ph: y})`)

The graph construction is done using the Python TensorFlow API (could also be done using the C++ TensorFlow API). Running the graph will call low-level C++ routines.

    '''
    function: create a linear model which try to fit the line 
              y = x + 2 using SGD optimizer to minimize 
              root-mean-square(RMS) loss function
    
    '''
    import tensorflow as tf
    import numpy as np

    # number of epoch
    num_epoch = 100

    # training data x and label y
    x = np.array([0., 1., 2., 3.], dtype=np.float32)
    y = np.array([2., 3., 4., 5.], dtype=np.float32)

    # convert x and y to 4x1 matrix
    x = np.reshape(x, [4, 1])
    y = np.reshape(y, [4, 1])

    # test set(using a little trick)
    x_test = x + 0.5
    y_test = y + 0.5

    # This part of the script builds the TensorFlow graph using the Python API

    # First declare placeholders for input x and label y
    # Placeholders are TensorFlow variables requiring to be explicitly fed by some 
    # input data
    x_ph = tf.placeholder(tf.float32, shape=[None, 1])
    y_ph = tf.placeholder(tf.float32, shape=[None, 1])

    # Variables (if not specified) will be learnt as the GradientDescentOptimizer
    # is run
    # Declare weight variable initialized using a truncated_normal law
    W = tf.Variable(tf.truncated_normal([1, 1], stddev=0.1))
    # Declare bias variable initialized to a constant 0.1
    b = tf.Variable(tf.constant(0.1, shape=[1]))

    # Initialize variables just declared 
    init = tf.initialize_all_variables()

    # In this part of the script, we build operators storing operations
    # on the previous variables and placeholders.
    # model: y = w * x + b
    y_pred = x_ph * W + b

    # loss function
    loss = tf.mul(tf.reduce_mean(tf.square(tf.sub(y_pred, y_ph))), 1. / 2)
    # create training graph
    train_op = tf.train.GradientDescentOptimizer(0.1).minimize(loss)

    # This part of the script runs the TensorFlow graph (variables and operations
    # operators) just built.
    with tf.Session() as sess:
        # initialize all the variables by running the initializer operator
        sess.run(init)
        for epoch in xrange(num_epoch):
            # Run sequentially the train_op and loss operators with
            # x_ph and y_ph placeholders fed by variables x and y
            _, loss_val = sess.run([train_op, loss], feed_dict={x_ph: x, y_ph: y})
            print('epoch %d: loss is %.4f' % (epoch, loss_val))

        # see what model do in the test set
        # by evaluating the y_pred operator using the x_test data
        test_val = sess.run(y_pred, feed_dict={x_ph: x_test})
        print('ground truth y is: %s' % y_test.flatten())
        print('predict y is     : %s' % test_val.flatten())


  [1]: https://www.wikiod.com/tensorflow/placeholders#Basics of Placeholders
  [2]: https://www.wikiod.com/tensorflow/variables#Declaring and Initializing Variable Tensors

## Counting to 10
In this example we use Tensorflow to count to 10.  **Yes** this is total overkill, but it is a nice example to show an absolute minimal setup needed to use Tensorflow

    import tensorflow as tf

    # create a variable, refer to it as 'state' and set it to 0
    state = tf.Variable(0)

    # set one to a constant set to 1
    one = tf.constant(1)

    # update phase adds state and one and then assigns to state
    addition = tf.add(state, one)
    update = tf.assign(state, addition )

    # create a session
    with tf.Session() as sess:
      # initialize session variables
      sess.run( tf.global_variables_initializer() )

      print "The starting state is",sess.run(state)

      print "Run the update 10 times..."
      for count in range(10):
        # execute the update
        sess.run(update)

      print "The end state is",sess.run(state)

The important thing to realize here is that **state, one, addition, and update** don't actually contain values.  Instead they are references to Tensorflow objects.  The final result is not **state**, but instead is retrieved by using a Tensorflow to evaluate it using **sess.run(state)**

This example is from https://github.com/panchishin/learn-to-tensorflow .  There are several other examples there and a nice graduated learning plan to get acquainted with manipulating the Tensorflow graph in python.


## Installation or Setup
As of Tensorflow version 1.0 installation has become much easier to perform.  At minimum to install TensorFlow one needs pip installed on their machine with a python version of at least 2.7 or 3.3+.  

    pip install --upgrade tensorflow      # for Python 2.7
    pip3 install --upgrade tensorflow     # for Python 3.n

For tensorflow on a GPU machine (as of 1.0 requires CUDA 8.0 and cudnn 5.1, AMD GPU not supported)

    pip install --upgrade tensorflow-gpu  # for Python 2.7 and GPU
    pip3 install --upgrade tensorflow-gpu # for Python 3.n and GPU


To test if it worked open up the correct version of python 2 or 3 and run

    import tensorflow

If that succeeded without error then you have tensorflow installed on your machine.

<br>

-----
*Be aware this references the master branch one can change this on the link above to reference the current stable release.)

  [1]: https://www.tensorflow.org
  [2]: https://www.tensorflow.org/versions/master/get_started/os_setup.html

## Tensorflow Basics
Tensorflow works on principle of dataflow graphs. To perform some computation there are two steps:
1. Represent the computation as a graph.
2. Execute the graph.

**Representation:**
Like any directed graph a Tensorflow graph consists of nodes and directional edges.

Node: A Node is also called an Op(stands for operation). An node can have multiple incoming edges but single outgoing edge.

Edge: Indicate incoming or outgoing data from a Node. In this case input(s) and output of some Node(Op).

Whenever we say data we mean an n-dimensional vector known as Tensor. A Tensor has three properties: *Rank, Shape and Type*

 - *Rank* means number of dimensions of the Tensor(a cube or box has rank 3).
 - *Shape* means values of those dimensions(box can have shape 1x1x1 or 2x5x7).
 - *Type* means datatype in each coordinate of Tensor.

**Execution:**
Even though a graph is constructed it is still an abstract entity. No computation actually occurs until we run it. To run a graph, we need to allocate CPU resource to Ops inside the graph. This is done using Tensorflow Sessions. Steps are:
1. Create a new session.
2. Run any Op inside the Graph. Usually we run the final Op where we expect the output of our computation.

An incoming edge on an Op is like a dependency for data on another Op. Thus when we run any Op, all incoming edges on it are traced and the ops on other side are also run.

**Note:** Special nodes called playing role of data source or sink are also possible. For example you can have an Op which gives a constant value thus no incoming edges(refer value 'matrix1' in the example below) and similarly Op with no outgoing edges where results are collected(refer value 'product' in the example below).

**Example:**

[![Example graph][1]][1]


<!-- language: lang-py -->

    import tensorflow as tf
    
    # Create a Constant op that produces a 1x2 matrix.  The op is
    # added as a node to the default graph.
    #
    # The value returned by the constructor represents the output
    # of the Constant op.
    matrix1 = tf.constant([[3., 3.]])
    
    # Create another Constant that produces a 2x1 matrix.
    matrix2 = tf.constant([[2.],[2.]])
    
    # Create a Matmul op that takes 'matrix1' and 'matrix2' as inputs.
    # The returned value, 'product', represents the result of the matrix
    # multiplication.
    product = tf.matmul(matrix1, matrix2)
    
    # Launch the default graph.
    sess = tf.Session()
    
    # To run the matmul op we call the session 'run()' method, passing 'product'
    # which represents the output of the matmul op.  This indicates to the call
    # that we want to get the output of the matmul op back.
    #
    # All inputs needed by the op are run automatically by the session.  They
    # typically are run in parallel.
    #
    # The call 'run(product)' thus causes the execution of three ops in the
    # graph: the two constants and matmul.
    #
    # The output of the op is returned in 'result' as a numpy `ndarray` object.
    result = sess.run(product)
    print(result)
    # ==> [[ 12.]]
    
    # Close the Session when we're done.
    sess.close()


  [1]: http://i.stack.imgur.com/2rIm0.jpg

