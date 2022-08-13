---
title: "Create a simple Sequential Model"
slug: "create-a-simple-sequential-model"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

The `Sequential` model is a linear stack of layers.

## Simple Multi Layer Perceptron wtih Sequential Models
You can create a Sequential model by passing a list of layer instances to the constructor:

    from keras.models import Sequential
    from keras.layers import Dense, Activation
    
    model = Sequential([
        Dense(32, input_dim=784),
        Activation('relu'),
        Dense(10),
        Activation('softmax'),
    ])

You can also simply add layers via the `.add()` method:

    model = Sequential()
    model.add(Dense(32, input_dim=784))
    model.add(Activation('relu'))

Models must be compiled before use:

    model.compile(loss='binary_crossentropy',
                  optimizer='sgd',
                  metrics=['accuracy'])


