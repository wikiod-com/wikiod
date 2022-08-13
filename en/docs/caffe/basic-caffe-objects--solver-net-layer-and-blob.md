---
title: "Basic Caffe Objects - Solver, Net, Layer and Blob"
slug: "basic-caffe-objects---solver-net-layer-and-blob"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

A caffe user sends instructions to perform specific operations to caffe objects. These objects interact with each other based on their design specifications and carry out the operation(s). This is a basic  principle OOP paradigm. 

While there are many caffe object types (or C++ classes), for a beginning basic understanding we focus upon 4 important caffe objects. Our objective at this stage is to simply observe the interaction between these objects on a highly abstracted level where specific implementation and design details are hazed out, and instead a bird's eye view of operation is focussed upon. 

The 4 basic caffe objects are :

 - **Solver**
 - **Net**
 - **Layer**
 - **Blob**

A very basic introduction and a bird's eye view of their role in the working of caffe is presented in concise points in the examples section.


After reading and getting a basic idea of how these caffe objects interact, each object type can be read about in detail in their dedicated topics.

## How these objects interact together.
 - A user is looking to use caffe for CNN training and testing. The user decides upon the CNN architecture design (e.g - No. of layers, No. of filters and their details etc). The user also decides the optimization technique for training and  learning parameters in case training is to be carried out. If the operation is of plain vanilla testing, a pre-trained model is specified by the user. Using all this information, the user instantiates a Solver object and provides the Solver object with an instruction (which decides operation(s) such as training and testing).
 - **Solver** : This object can be looked upon as an entity that oversees the training and testing of a CNN. It is the actual contractor who gets a CNN up on processor and running. It is specialised in carrying out the specific optimizations that lead to a CNN getting trained.
 - **Net** : Net can be thought of as a specialist object that represents the actual CNN over which operation(s) are carried out. Net is instructed by Solver to actually allocate memory for the CNN and instantiate it. Net is also responsible for giving instructions which actually lead to forward or backpropagation being carried out over the CNN.
 - **Layer** : It is an object that represents a particular layer of a CNN. Thus a CNN is made up of layers. As far as caffe is concerned, **Net** object instantiates each "**Layer**" type specified in the architecture definition and it also connects different layers together. A specific layer carries out a specific set of operation(s) (e.g - Max-Pooling, Min-Pooling, 2D Convolution etc.)

 - **Blob** : Data flows through a CNN during training and testing. This data apart from containing user data, also includes several intermediate computations that are performed over CNN. This data is encapsulated in an object called Blob.


