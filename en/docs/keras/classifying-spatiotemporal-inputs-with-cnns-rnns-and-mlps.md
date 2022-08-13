---
title: "Classifying Spatiotemporal Inputs with CNNs, RNNs, and MLPs"
slug: "classifying-spatiotemporal-inputs-with-cnns-rnns-and-mlps"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Spatiotemporal data, or data with spatial and temporal qualities, are a common occurrence. Examples include videos, as well as sequences of image-like data, such as spectrograms. 

Convolutional Neural Networks (CNNs) are particularly suited for finding spatial patterns. Recurrent Neural Networks (RNNs), on the other hand, are particularly suited for finding temporal patterns. These two, in combination with Multilayer Perceptrons, can be effective for classifying spatiotemporal inputs.

In this example, a VGG-16 model pre-trained on the ImageNet database was used. If a trainable VGG-16 model is desired, set the VGG-16 `weights` parameter to `None` for random initialization and set the `cnn.trainable` attribute to `True`.

The number and kind of layers, units, and other parameters should be tweaked as necessary for specific application needs.

## VGG-16 CNN and LSTM for Video Classification
For this example, let's assume that the inputs have a dimensionality of *(frames, channels, rows, columns)*, and the outputs have a dimensionality of *(classes)*.

    from keras.applications.vgg16 import VGG16
    from keras.models import Model
    from keras.layers import Dense, Input
    from keras.layers.pooling import GlobalAveragePooling2D
    from keras.layers.recurrent import LSTM
    from keras.layers.wrappers import TimeDistributed
    from keras.optimizers import Nadam

    video = Input(shape=(frames,
                         channels,
                         rows,
                         columns))
    cnn_base = VGG16(input_shape=(channels,
                                  rows,
                                  columns),
                     weights="imagenet",
                     include_top=False)
    cnn_out = GlobalAveragePooling2D()(cnn_base.output)
    cnn = Model(input=cnn_base.input, output=cnn_out)
    cnn.trainable = False
    encoded_frames = TimeDistributed(cnn)(video)
    encoded_sequence = LSTM(256)(encoded_frames)
    hidden_layer = Dense(output_dim=1024, activation="relu")(encoded_sequence)
    outputs = Dense(output_dim=classes, activation="softmax")(hidden_layer)
    model = Model([video], outputs)
    optimizer = Nadam(lr=0.002,
                      beta_1=0.9,
                      beta_2=0.999,
                      epsilon=1e-08,
                      schedule_decay=0.004)
    model.compile(loss="categorical_crossentropy",
                  optimizer=optimizer,
                  metrics=["categorical_accuracy"]) 

