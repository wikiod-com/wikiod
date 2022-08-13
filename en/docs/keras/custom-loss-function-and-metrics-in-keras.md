---
title: "Custom loss function and metrics in Keras"
slug: "custom-loss-function-and-metrics-in-keras"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

You can create a custom loss function and metrics in Keras by defining a TensorFlow/Theano symbolic function that returns a scalar for each data-point and takes the following two arguments: tensor of true values, tensor of the corresponding predicted values.

Note that the loss/metric (for display and optimization) is calculated as the mean of the losses/metric across all datapoints in the batch.

Keras loss functions are defined in [losses.py](https://github.com/fchollet/keras/blob/master/keras/losses.py)

Additional loss functions for Keras can be found in [keras-contrib](https://github.com/farizrahman4u/keras-contrib) repository. 



## Euclidean distance loss
<!-- language-all: lang-python -->
Define a custom loss function:

    import keras.backend as K


    def euclidean_distance_loss(y_true, y_pred):
        """
        Euclidean distance loss
        https://en.wikipedia.org/wiki/Euclidean_distance
        :param y_true: TensorFlow/Theano tensor
        :param y_pred: TensorFlow/Theano tensor of the same shape as y_true
        :return: float
        """
        return K.sqrt(K.sum(K.square(y_pred - y_true), axis=-1))

Use it:

    model.compile(loss=euclidean_distance_loss, optimizer='rmsprop')

