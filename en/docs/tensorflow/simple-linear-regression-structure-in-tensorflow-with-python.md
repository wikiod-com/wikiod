---
title: "Simple linear regression structure in TensorFlow with Python"
slug: "simple-linear-regression-structure-in-tensorflow-with-python"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

A model widely used in traditional statistics is the linear regression model. In this article, the objective is to follow the step-by-step implementation of this type of models. We are going to represent a simple linear regression structure.

For our study, we will analyze the age of the children on the **x** axis and the height of the children on the **y** axis. We will try to predict the height of the children, using their age, applying simple linear regression.[in TF finding the best W and b]

## Parameters
Parameter | Description
------ | ------
train_X   | np array with x dimension of information 
train_Y   | np array with y dimension of information

I used TensorBoard sintaxis to track the behavior of some parts of the model, cost, train and activation elements.

    with tf.name_scope("") as scope:

Imports used:

    import numpy as np
    import tensorflow as tf


----------


Type of application and language used:

> I have used a traditional console implementation app type, developed in Python, to represent the example.


----------

Version of TensorFlow used:

> 1.0.1


----------
Conceptual **academic** example/reference extracted from [here][1]:  


  [1]: http://openclassroom.stanford.edu/MainFolder/DocumentPage.php?course=MachineLearning&doc=exercises/ex2/ex2.html

## Simple regression function code structure
Function definition:

    def run_training(train_X, train_Y):

Inputs variables:

    X = tf.placeholder(tf.float32, [m, n])
    Y = tf.placeholder(tf.float32, [m, 1])


----------


 Weight and bias representation

    W = tf.Variable(tf.zeros([n, 1], dtype=np.float32), name="weight")
    b = tf.Variable(tf.zeros([1], dtype=np.float32), name="bias")


----------


Lineal Model:

    with tf.name_scope("linear_Wx_b") as scope:
        activation = tf.add(tf.matmul(X, W), b)

----------

Cost:

    with tf.name_scope("cost") as scope:
        cost = tf.reduce_sum(tf.square(activation - Y)) / (2 * m)
        tf.summary.scalar("cost", cost)

----------

Training:

    with tf.name_scope("train") as scope:
        optimizer = tf.train.GradientDescentOptimizer(0.07).minimize(cost)

----------

TensorFlow session:

    with tf.Session() as sess:
        merged = tf.summary.merge_all()
        writer = tf.summary.FileWriter(log_file, sess.graph)

Note: **merged** and writer are part of the TensorBoard strategy to track the model behavior.


----------


        init = tf.global_variables_initializer()
        sess.run(init)


----------


Repeating 1.5k times the training loop:
 
        for step in range(1500):
           result, _ = sess.run([merged, optimizer], feed_dict={X: np.asarray(train_X), Y: np.asarray(train_Y)})
           writer.add_summary(result, step)


----------
Print Training Cost:

        training_cost = sess.run(cost, feed_dict={X: np.asarray(train_X), Y: np.asarray(train_Y)})
        print "Training Cost: ", training_cost, "W=", sess.run(W), "b=", sess.run(b), '\n'


----------
Concrete prediction based on the model trained:

        print "Prediction for 3.5 years"
        predict_X = np.array([3.5], dtype=np.float32).reshape([1, 1])

        predict_X = (predict_X - mean) / std
        predict_Y = tf.add(tf.matmul(predict_X, W), b)
        print "Child height(Y) =", sess.run(predict_Y)




## Main Routine
    def main():
        train_X, train_Y = read_data()
        train_X = feature_normalize(train_X)
        run_training(train_X, train_Y)

Note: remember review functions dependencies. **read_data**, **feature_normalize** and **run_training**

## Normalization Routine
    def feature_normalize(train_X):
        global mean, std
        mean = np.mean(train_X, axis=0)
        std = np.std(train_X, axis=0)

        return np.nan_to_num((train_X - mean) / std)

## Read Data routine
    def read_data():
        global m, n
    
        m = 50
        n = 1
    
        train_X = np.array(

Internal data for the array

        ).astype('float32')
    
        train_Y = np.array(

Internal data for the array

        ).astype('float32')
    
        return train_X, train_Y

