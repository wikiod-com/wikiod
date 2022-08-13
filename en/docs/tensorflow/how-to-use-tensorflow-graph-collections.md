---
title: "How to use TensorFlow Graph Collections?"
slug: "how-to-use-tensorflow-graph-collections"
draft: false
images: []
weight: 8888
type: docs
toc: true
---

When you have huge model, it is useful to form some groups of tensors in your computational graph, that are connected with each other. For example tf.GraphKeys class contains such standart collections as:

    tf.GraphKeys.VARIABLES
    tf.GraphKeys.TRAINABLE_VARIABLES
    tf.GraphKeys.SUMMARIES

## Create your own collection and use it to collect all your losses.
Here we will create collection for losses of Neural Network's computational graph.

First create a computational graph like so: 

    with tf.variable_scope("Layer"):
        W = tf.get_variable("weights", [m, k],
            initializer=tf.zeros_initializer([m, k], dtype=tf.float32))
        b1 = tf.get_variable("bias", [k],
            initializer = tf.zeros_initializer([k], dtype=tf.float32))
        z = tf.sigmoid((tf.matmul(input, W) + b1))
        
        with tf.variable_scope("Softmax"):
            U = tf.get_variable("weights", [k, r],
                initializer=tf.zeros_initializer([k,r], dtype=tf.float32))
            b2 = tf.get_variable("bias", [r],
                initializer=tf.zeros_initializer([r], dtype=tf.float32))
        out = tf.matmul(z, U) + b2
    cross_entropy = tf.reduce_mean(
        tf.nn.sparse_softmax_cross_entropy_with_logits(out, labels))
To create a new collection, you can simply start calling `tf.add_to_collection()` - the first call will create the collection. 
        

    tf.add_to_collection("my_losses", 
        self.config.l2 * (tf.add_n([tf.reduce_sum(U ** 2), tf.reduce_sum(W ** 2)])))
    tf.add_to_collection("my_losses", cross_entropy)
And finally you can get tensors from your collection: 

    loss = sum(tf.get_collection("my_losses"))

Note that `tf.get_collection()` returns a copy of the collection or an empty list if the collection does not exist. Also, it does NOT create the collection if it does not exist. To do so, you could use `tf.get_collection_ref()` which returns a reference to the collection and actually creates an empty one if it does not exist yet.

## Collect variables from nested scopes
Below is a single hidden layer Multilayer Perceptron (MLP) using nested scoping of variables. 


    def weight_variable(shape):
        return tf.get_variable(name="weights", shape=shape,
                               initializer=tf.zeros_initializer(dtype=tf.float32))

    def bias_variable(shape):
        return tf.get_variable(name="biases", shape=shape,
                               initializer=tf.zeros_initializer(dtype=tf.float32))

    def fc_layer(input, in_dim, out_dim, layer_name):
        with tf.variable_scope(layer_name):
            W = weight_variable([in_dim, out_dim])
            b = bias_variable([out_dim])
            linear = tf.matmul(input, W) + b
            output = tf.sigmoid(linear)

    with tf.variable_scope("MLP"):
        x = tf.placeholder(dtype=tf.float32, shape=[None, 1], name="x")
        y = tf.placeholder(dtype=tf.float32, shape=[None, 1], name="y")
        fc1 = fc_layer(x, 1, 8, "fc1")
        fc2 = fc_layer(fc1, 8, 1, "fc2")

    mse_loss = tf.reduce_mean(tf.reduce_sum(tf.square(fc2 - y), axis=1))

The MLP uses the the top level scope name `MLP` and it has two layers with their respective scope names `fc1` and `fc2`. Each layer also has its own `weights` and `biases` variables.

The variables can be collected like so:
    
    trainable_var_key = tf.GraphKeys.TRAINABLE_VARIABLES
    all_vars = tf.get_collection(key=trainable_var_key, scope="MLP")
    fc1_vars = tf.get_collection(key=trainable_var_key, scope="MLP/fc1")
    fc2_vars = tf.get_collection(key=trainable_var_key, scope="MLP/fc2")
    fc1_weight_vars = tf.get_collection(key=trainable_var_key, scope="MLP/fc1/weights")
    fc1_bias_vars = tf.get_collection(key=trainable_var_key, scope="MLP/fc1/biases")

The values of the variables can be collected using the `sess.run()` command. For example if we would like to collect the values of the `fc1_weight_vars` after training, we could do the following:

    sess = tf.Session()
    # add code to initialize variables
    # add code to train the network
    # add code to create test data x_test and y_test

    fc1_weight_vals = sess.run(fc1, feed_dict={x: x_test, y: y_test})
    print(fc1_weight_vals)  # This should be an ndarray with ndim=2 and shape=[1, 8]

        
        

