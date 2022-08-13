---
title: "Neural Networks"
slug: "neural-networks"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Activation Functions
Activation functions also known as transfer function is used to map input nodes to output nodes in certain fashion.

They are used to impart non linearity to the output of a neural network layer.

Some commonly used functions and their curves are given below:
[![Activation functions][1]][1]

----

### Sigmoid Function

The sigmoid is a squashing function whose output is in the range `[0, 1]`.

[![enter image description here][2]][2]

The code for implementing sigmoid along with its derivative with `numpy` is shown below:

    def sigmoid(z):
        return 1.0 / (1.0 + np.exp(-z))
    
    def sigmoid_prime(z):
        return sigmoid(z) * (1 - sigmoid(z))

### Hyperbolic Tangent Function (tanh)

The basic difference between the tanh and sigmoid functions is that tanh is 0 centred, squashing inputs into the range [-1, 1] and is more efficient to compute.

[![enter image description here][3]][3]

You can easily use the `np.tanh` or `math.tanh` functions to compute the activation of a hidden layer. 

### ReLU Function

A rectified linear unit does simply `max(0,x)`. It is the one of the most common choices for activation functions of neural network units.

[![enter image description here][4]][4]

ReLUs address the [vanishing gradient problem][5] of sigmoid / hyperbolic tangent units, thus allowing for efficient gradient propagation in deep networks.

The name ReLU comes from Nair and Hinton's paper, *[Rectified Linear Units Improve Restricted Boltzmann Machines][6]*.

It has some variations, for example, leaky ReLUs (LReLUs) and Exponential Linear Units (ELUs).

The code for implementing vanilla ReLU along with its derivative with `numpy` is shown below:

    def relU(z):
        return z * (z > 0)
    
    def relU_prime(z):
        return z > 0


  [1]: https://i.stack.imgur.com/D8ByS.png
  [2]: https://i.stack.imgur.com/YUC5S.png
  [3]: https://i.stack.imgur.com/MpzDY.jpg
  [4]: http://i.stack.imgur.com/5wnDj.png
  [5]: https://en.wikipedia.org/wiki/Vanishing_gradient_problem
  [6]: http://www.cs.toronto.edu/~fritz/absps/reluICML.pdf

## Getting Started: A Simple ANN with Python
The code listing below attempts to classify handwritten digits from the MNIST dataset. The digits look like this:

[![MNIST][1]][1]

The code will preprocess these digits, converting each image into a 2D array of 0s and 1s, and then use this data to train a neural network with upto 97% accuracy (50 epochs).

    """
    Deep Neural Net 

    (Name: Classic Feedforward)
    
    """
    
    import numpy as np
    import pickle, json
    import sklearn.datasets
    import random
    import time
    import os
    
    
    def sigmoid(z):
        return 1.0 / (1.0 + np.exp(-z))
    
    def sigmoid_prime(z):
        return sigmoid(z) * (1 - sigmoid(z))
    
    def relU(z):
        return np.maximum(z, 0, z)
        
    def relU_prime(z):
        return z * (z <= 0)
        
    def tanh(z):
        return np.tanh(z)
    
    def tanh_prime(z):
        return 1 - (tanh(z) ** 2)
    
    def transform_target(y):
        t = np.zeros((10, 1))
        t[int(y)] = 1.0
        return t
    
    """--------------------------------------------------------------------------------"""
    
    class NeuralNet:
    
        def __init__(self, layers, learning_rate=0.05, reg_lambda=0.01):
            self.num_layers = len(layers)
            self.layers = layers
            self.biases = [np.zeros((y, 1)) for y in layers[1:]]
            self.weights = [np.random.normal(loc=0.0, scale=0.1, size=(y, x)) for x, y in zip(layers[:-1], layers[1:])]
            self.learning_rate = learning_rate
            self.reg_lambda = reg_lambda
            self.nonlinearity = relU
            self.nonlinearity_prime = relU_prime
    
        def __feedforward(self, x):
            """ Returns softmax probabilities for the output layer """
            for w, b in zip(self.weights, self.biases):
                x = self.nonlinearity(np.dot(w, np.reshape(x, (len(x), 1))) + b)
    
            return np.exp(x) / np.sum(np.exp(x))
    
        def __backpropagation(self, x, y):
            """
            :param x: input
            :param y: target
            """
            weight_gradients = [np.zeros(w.shape) for w in self.weights]
            bias_gradients = [np.zeros(b.shape) for b in self.biases]
    
            # forward pass
            activation = x
            hidden_activations = [np.reshape(x, (len(x), 1))]
            z_list = []
    
            for w, b in zip(self.weights, self.biases):    
                z = np.dot(w, np.reshape(activation, (len(activation), 1))) + b
                z_list.append(z)
                activation = self.nonlinearity(z)
                hidden_activations.append(activation)
    
            t = hidden_activations[-1] 
            hidden_activations[-1] = np.exp(t) / np.sum(np.exp(t))
    
            # backward pass
            delta = (hidden_activations[-1] - y) * (z_list[-1] > 0)
            weight_gradients[-1] = np.dot(delta, hidden_activations[-2].T)
            bias_gradients[-1] = delta
    
            for l in range(2, self.num_layers):
                z = z_list[-l]
                delta = np.dot(self.weights[-l + 1].T, delta) * (z > 0)
                weight_gradients[-l] = np.dot(delta, hidden_activations[-l - 1].T)
                bias_gradients[-l] = delta
    
            return (weight_gradients, bias_gradients)
    
        def __update_params(self, weight_gradients, bias_gradients):
            for i in xrange(len(self.weights)):
                self.weights[i] += -self.learning_rate * weight_gradients[i]
                self.biases[i] += -self.learning_rate * bias_gradients[i]
    
        def train(self, training_data, validation_data=None, epochs=10):
            bias_gradients = None
            for i in xrange(epochs):
                random.shuffle(training_data)
                inputs = [data[0] for data in training_data]
                targets = [data[1] for data in training_data]
    
                for j in xrange(len(inputs)):
                    (weight_gradients, bias_gradients) = self.__backpropagation(inputs[j], targets[j])
                    self.__update_params(weight_gradients, bias_gradients)
    
                if validation_data: 
                    random.shuffle(validation_data)
                    inputs = [data[0] for data in validation_data]
                    targets = [data[1] for data in validation_data]
    
                    for j in xrange(len(inputs)):
                        (weight_gradients, bias_gradients) = self.__backpropagation(inputs[j], targets[j])
                        self.__update_params(weight_gradients, bias_gradients)
    
                print("{} epoch(s) done".format(i + 1))
    
            print("Training done.")
    
        def test(self, test_data):
            test_results = [(np.argmax(self.__feedforward(x[0])), np.argmax(x[1])) for x in test_data]
            return float(sum([int(x == y) for (x, y) in test_results])) / len(test_data) * 100
    
        def dump(self, file):
            pickle.dump(self, open(file, "wb"))
    
    """--------------------------------------------------------------------------------"""
    
    if __name__ == "__main__":
        total = 5000
        training = int(total * 0.7)
        val = int(total * 0.15)
        test = int(total * 0.15)
    
        mnist = sklearn.datasets.fetch_mldata('MNIST original', data_home='./data')
    
        data = zip(mnist.data, mnist.target)
        random.shuffle(data)
        data = data[:total]
        data = [(x[0].astype(bool).astype(int), transform_target(x[1])) for x in data]
    
        train_data = data[:training]
        val_data = data[training:training+val]
        test_data = data[training+val:]
    
        print "Data fetched"
    
        NN = NeuralNet([784, 32, 10]) # defining an ANN with 1 input layer (size 784 = size of the image flattened), 1 hidden layer (size 32), and 1 output layer (size 10, unit at index i will predict the probability of the image being digit i, where 0 <= i <= 9)  
    
        NN.train(train_data, val_data, epochs=5)
    
        print "Network trained"
    
        print "Accuracy:", str(NN.test(test_data)) + "%"

This is a self contained code sample, and can be run without any further modifications. Ensure you have `numpy` and `scikit` learn installed for your version of python.

  [1]: https://i.stack.imgur.com/gSei6.png


## Backpropagation - The Heart of Neural Networks
The goal of backpropagation is to optimize the weights so that the neural network can learn how to correctly map arbitrary inputs to outputs.

Each layer has its own set of weights, and these weights must be tuned to be able to accurately predict the right output given input. 

A high level overview of back propagation is as follows:

 1. Forward pass - the input is transformed into some output. At each layer, the activation is computed with a dot product between the input and the weights, followed by summing the resultant with the bias. Finally, this value is passed through an activation function, to get the activation of that layer which will become the input to the next layer. 
 2. In the last layer, the output is compared to the actual label corresponding to that input, and the error is computed. Usually, it is the mean squared error.
 3. Backward pass - the error computed in step 2 is propagated back to the inner layers, and the weights of all layers are adjusted to account for this error.

## 1. Weights Initialisation

A simplified example of weights initialisation is shown below:

    layers = [784, 64, 10] 
    weights = np.array([(np.random.randn(y, x) * np.sqrt(2.0 / (x + y))) for x, y in zip(layers[:-1], layers[1:])])
    biases = np.array([np.zeros((y, 1)) for y in layers[1:]])

 - Hidden layer 1 has weight of dimension [64, 784] and bias of
   dimension 64.
   
 - Output layer has weight of dimension [10, 64] and bias of dimension
   10.

You may be wondering what is going on when initialising weights in the code above. This is called Xavier initialisation, and it is a step better than randomly initialising your weight matrices. Yes, initialisation does matter. Based on your initialisation, you might be able to find a better local minima during gradient descent (back propagation is a glorified version of gradient descent). 

## 2. Forward Pass

    activation = x
    hidden_activations = [np.reshape(x, (len(x), 1))]
    z_list = []

    for w, b in zip(self.weights, self.biases):    
        z = np.dot(w, np.reshape(activation, (len(activation), 1))) + b
        z_list.append(z)
        activation = relu(z)
        hidden_activations.append(activation)

    t = hidden_activations[-1] 
    hidden_activations[-1] = np.exp(t) / np.sum(np.exp(t))

This code carries out the transformation described above. `hidden_activations[-1]` contains softmax probabilities - predictions of all classes, the sum of which is 1. If we are predicting digits, then output will be a vector of probabilities of dimension 10, the sum of which is 1.

## 3. Backward Pass
    weight_gradients = [np.zeros(w.shape) for w in self.weights]
    bias_gradients = [np.zeros(b.shape) for b in self.biases]

    delta = (hidden_activations[-1] - y) * (z_list[-1] > 0) # relu derivative
    weight_gradients[-1] = np.dot(delta, hidden_activations[-2].T)
    bias_gradients[-1] = delta

    for l in range(2, self.num_layers):
        z = z_list[-l]
        delta = np.dot(self.weights[-l + 1].T, delta) * (z > 0) # relu derivative
        weight_gradients[-l] = np.dot(delta, hidden_activations[-l - 1].T)
        bias_gradients[-l] = delta

The first 2 lines initialise the gradients. These gradients are computed and will be used to update the weights and biases later.

The next 3 lines compute the error by subtracting the prediction from the target. The error is then back propagated to the inner layers. 

Now, carefully trace the working of the loop. Lines 2 and 3 transform the error from `layer[i]` to `layer[i - 1]`. Trace the shapes of the matrices being multiplied to understand.

## 4. Weights/Parameter Update

    for i in xrange(len(self.weights)):
        self.weights[i] += -self.learning_rate * weight_gradients[i]
        self.biases[i] += -self.learning_rate * bias_gradients[i] 

`self.learning_rate` specifies the rate at which the network learns. You don't want it to learn too fast, because it may not converge. A smooth descent is favoured for finding a good minima. Usually, rates between `0.01` and `0.1` are considered good.


    

## Softmax Function
Softmax regression (or multinomial logistic regression) is a generalization of logistic regression to the case where we want to handle multiple classes. It is particularly useful for neural networks where we want to apply non-binary classification. In this case, simple logistic regression is not sufficient. We'd need a probability distribution across all labels, which is what softmax gives us.

Softmax is computed with the below formula:

 [![Formula][1]][1]

   ### ___________________________Where does it fit in? _____________________________
[![Softmax][2]][2]
To normalise a vector by applying the softmax function to it with `numpy`, use:

    np.exp(x) / np.sum(np.exp(x))

Where `x` is the activation from the final layer of the ANN.


  [1]: https://i.stack.imgur.com/wxJKQ.jpg
  [2]: https://i.stack.imgur.com/9pQIm.png

