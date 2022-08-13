---
title: "Deep Learning"
slug: "deep-learning"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

Deep Learning is a sub-field of machine learning where multi-layer artificial neural networks are used for learning purpose.  Deep Learning has found lots of great implementations, e.g. Speech Recognition, Subtitles on Youtube, Amazon recommendation, and so on. For additional information there is a dedicated topic to [deep-learning](https://www.wikiod.com/deep-learning).


## Short brief of Deep learning
To train a neural network, firstly we need to design a good and efficient idea.
There are three types of learning tasks.

 - Supervised Learning
 - Reinforcement Learning
 - Unsupervised Learning

In this present time, unsupervised learning is very popular.Unsupervised Learning is a deep learning task of inferring a function to describe hidden structure from "unlabeled" data (a classification or categorization is not included in the observations). 

Since the examples given to the learner are unlabeled, there is no evaluation of the accuracy of the structure that is output by the relevant algorithm—which is one way of distinguishing unsupervised learning from Supervised Learning and Reinforcement Learning.

There are three types of Unsupervised learning.

 - Restricted Boltzmann Machines 
 - Sparse Coding Model
 - Autoencoders
I will describe in detail of autoencoder.

The aim of an autoencoder is to learn a representation (encoding) for a set of data, typically for the purpose of dimensionality reduction.

The simplest form of an autoencoder is a feedforward, having an input layer, an output layer and one or more hidden layers connecting them.
But with the output layer having the same number of nodes as the input layer, and with the purpose of reconstructing its own inputs and that's why it is called unsupervised learning.

Now I will try to give an example of training neural network.
[![enter image description here][1]][1]


Here Xi is input, W is weight, f(e) is activation function and y is output.


Now we see step by step flow of training neural network based on autoencoder.
[![enter image description here][1]][1]

We calculate every activation function's value with this equation: y=WiXi.
First of all, we randomly pick numbers for weights and then try to adjust that weights.

[![enter image description here][2]][2]


[![enter image description here][3]][3]
[![enter image description here][4]][4]
[![enter image description here][5]][5]

Now, we calculate deviation from our desired output, that is y=z-y and calculate every activation function's deviations.

[![enter image description here][6]][6]

Then we adjust our new weight of every connections.

[![enter image description here][7]][7]


  [1]: https://i.stack.imgur.com/PLvIK.png
  [2]: https://i.stack.imgur.com/ffJj0.png
  [3]: https://i.stack.imgur.com/OMdWJ.png
  [4]: https://i.stack.imgur.com/Ethx6.png
  [5]: https://i.stack.imgur.com/zCRN8.png
  [6]: https://i.stack.imgur.com/pAWWf.png
  [7]: https://i.stack.imgur.com/w7UBm.png

