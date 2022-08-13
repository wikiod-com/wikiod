---
title: "Multidimensional softmax"
slug: "multidimensional-softmax"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Creating a Softmax Output Layer
When `state_below` is a 2D Tensor, `U` is a 2D weights matrix, `b` is a `class_size`-length vector:

    logits = tf.matmul(state_below, U) + b
    return tf.nn.softmax(logits)


When `state_below` is a 3D tensor, `U`, `b` as before:

    def softmax_fn(current_input):
        logits = tf.matmul(current_input, U) + b
        return tf.nn.softmax(logits)

    raw_preds = tf.map_fn(softmax_fn, state_below)

## Computing Costs on a Softmax Output Layer
Use [`tf.nn.sparse_softmax_cross_entropy_with_logits`](https://www.tensorflow.org/versions/r0.10/api_docs/python/nn.html#sparse_softmax_cross_entropy_with_logits), but beware that it can't accept the output of `tf.nn.softmax`. Instead, calculate the unscaled activations, and then the cost:

    logits = tf.matmul(state_below, U) + b
    cost = tf.nn.sparse_softmax_cross_entropy_with_logits(logits, labels)

In this case: `state_below` and `U` should be 2D matrices, `b` should be a vector of a size equal to the number of classes, and `labels` should be a 2D matrix of `int32` or `int64`. This function also supports activation tensors with more than two dimensions.

