---
title: "Getting started with keras"
slug: "getting-started-with-keras"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started with Keras : 30 Second
The core data structure of Keras is a **model**, a way to organize layers. The main type of model is the **[Sequential][1]** model, a linear stack of layers. For more complex architectures, you should use the [Keras functional API][2].

Here's the Sequential model:

    from keras.models import Sequential
    
    model = Sequential()

Stacking layers is as easy as `.add()`:

    from keras.layers import Dense, Activation
    
    model.add(Dense(output_dim=64, input_dim=100))
    model.add(Activation("relu"))
    model.add(Dense(output_dim=10))
    model.add(Activation("softmax"))

Once your model looks good, configure its learning process with `.compile()`:

    model.compile(loss='categorical_crossentropy', optimizer='sgd', metrics=['accuracy'])
If you need to, you can further configure your optimizer. A core principle of Keras is to make things reasonably simple, while allowing the user to be fully in control when they need to (the ultimate control being the easy extensibility of the source code).

    from keras.optimizers import SGD
    model.compile(loss='categorical_crossentropy', optimizer=SGD(lr=0.01, momentum=0.9, nesterov=True))

You can now iterate on your training data in batches:

    model.fit(X_train, Y_train, nb_epoch=5, batch_size=32)
Alternatively, you can feed batches to your model manually:

    model.train_on_batch(X_batch, Y_batch)
Evaluate your performance in one line:

    loss_and_metrics = model.evaluate(X_test, Y_test, batch_size=32)
Or generate predictions on new data:

    classes = model.predict_classes(X_test, batch_size=32)
    proba = model.predict_proba(X_test, batch_size=32)

Building a question answering system, an image classification model, a Neural Turing Machine, a word2vec embedder or any other model is just as fast. The ideas behind deep learning are simple, so why should their implementation be painful?

You will find more advanced models: question-answering with memory networks, text generation with stacked LSTMs, etc in [example folder][3].


  [1]: https://keras.io/getting-started/sequential-model-guide/
  [2]: http://keras.io/getting-started/functional-api-guide
  [3]: https://github.com/fchollet/keras/tree/master/examples

## Installation and Setup
Keras is a high-level neural networks library, written in Python and capable of running on top of either TensorFlow or Theano. It was developed with a focus on enabling fast experimentation. Being able to go from idea to result with the least possible delay is key to doing good research.
Use Keras if you need a deep learning library that:

 - Allows for easy and fast prototyping (through total modularity,
   minimalism, and extensibility).
 - Supports both convolutional networks and recurrent networks, as well
   as combinations of the two.
 - Supports arbitrary connectivity schemes (including multi-input and
   multi-output training).
 - Runs seamlessly on CPU and GPU.

------------------------

# Installation

Keras uses the following dependencies:

 - numpy, scipy
 - pyyaml
 - HDF5 and h5py (optional, required if you use model saving/loading
   functions)
 - Optional but recommended if you use CNNs: cuDNN
 - scikit-image (optional, required if you use keras built-in functions for preprocessing and augmenting image data)


Keras is a  high-level library that provides a convenient Machine Learning API on top of other low-level libraries for tensor processing and manipulation, called [*Backends*][1]. At this time, Keras can be used on top any of the three available backends: *TensorFlow*, *Theano*, and *CNTK*.


[**Theano**][3] is installed automatically if you install *Keras* using *pip*.
If you want to install *Theano* manually, please refer to *Theano* installation instructions.

[**TensorFlow**][2] is a recommended option, and by default, *Keras* uses *TensorFlow* backend, if available. To install *TensorFlow*, the easiest way is to do

    $ pip install tensorflow

If you want to install it manually, please refer to *TensorFlow* installation instructions.

To install *Keras*, cd to the *Keras* folder and run the install command:

    $ python setup.py install
You can also install Keras from PyPI:

    $ pip install keras

----------------------------

# Configuration

If you have run Keras at least once, you will find the Keras configuration file at:

    ~/.keras/keras.json

If it isn't there, you can create it. The default configuration file looks like this:

    {
        "image_dim_ordering": "tf",
        "epsilon": 1e-07,
        "floatx": "float32",
        "backend": "tensorflow"
    }  


## Switching from TensorFlow to Theano

By default, Keras will use TensorFlow as its tensor manipulation library. If you want to use other backend, simply change the field backend to either `"theano"` or `"tensorflow"`, and Keras will use the new configuration next time you run any Keras code.


  [1]: https://keras.io/backend/
  [2]: https://www.tensorflow.org/
  [3]: http://deeplearning.net/software/theano/

