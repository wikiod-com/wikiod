---
title: "Perceptron"
slug: "perceptron"
draft: false
images: []
weight: 9610
type: docs
toc: true
---

## Implementing a Perceptron model in C++
In this example I will go through the implementation of the perceptron model in C++ so that you can get a better idea of how it works.

First things first it is a good practice to write down a simple algorithm of what we want to do.

Algorithm:

 1. Make a the vector for the weights and initialize it to 0 (Don't forget to add the bias term)
 2. Keep adjusting the weights until we get 0 errors or a low error count.
 3. Make predictions on unseen data.

Having written a super simple algorithm let's now write some of the functions that we will need.

 - We will need a function to calculate the net's input (e.i ***x * wT*** multiplying the inputs time the weights)
 - A step function so that we get a prediction of either 1 or -1
 - And a function that finds the ideal values for the weights.

So without further ado let's get right into it.

Let's start simple by creating a perceptron class:

    class perceptron
    {
    public:
        
    private:

    };

Now let's add the functions that we will need.

    class perceptron
    {
    public:
        perceptron(float eta,int epochs);
        float netInput(vector<float> X);
        int predict(vector<float> X);
        void fit(vector< vector<float> > X, vector<float> y);
    private:

    };

Notice how the function ***fit*** takes as an argument a vector of vector< float >. That is because our training dataset is a matrix of inputs. Essentially we can imagine that matrix as a couple of vectors ***x*** stacked the one on top of another and each column of that Matrix being a feature.

Finally let's add the values that our class needs to have. Such as the vector ***w*** to hold the weights, the number of ***epochs*** which indicates the number of passes that we will do over the training dataset. And the constant ***eta*** which is the learning rate of which we will multiply each weight update in order to make the training procedure faster by dialing this value up or if ***eta*** is too high we can dial it down to get the ideal result( for most applications of the perceptron I would suggest an ***eta*** value of 0.1 ).

    class perceptron
    {
    public:
        perceptron(float eta,int epochs);
        float netInput(vector<float> X);
        int predict(vector<float> X);
        void fit(vector< vector<float> > X, vector<float> y);
    private:
        float m_eta;
        int m_epochs;
        vector < float > m_w;
    };

Now with our class set. It's time to write each one of the functions.

We will start from the constructor  ( **perceptron(float eta,int epochs);** )

    perceptron::perceptron(float eta, int epochs)
    {
        m_epochs = epochs; // We set the private variable m_epochs to the user selected value
        m_eta = eta; // We do the same thing for eta
    }
As you can see what we will be doing is very simple stuff. So let's move on to another simple function. The predict function( ***int predict(vector<float> X);*** ). Remember that what the all ***predict*** function does is taking the net input and returning a value of 1 if the ***netInput*** is bigger than 0 and -1 otherwhise.

    int perceptron::predict(vector<float> X)
    {
        return netInput(X) > 0 ? 1 : -1; //Step Function
    }
Notice that we used an inline if statement to make our lives easier. Here's how the inline if statement works:

***condition ? if_true : else***

So far so good. Let's move on to implementing the ***netInput*** function( ***float netInput(vector<float> X);*** ) 

The netInput does the following; ***multiplies the input vector by the transpose of the weights vector***

***x * wT*** 

In other words, it multiplies each element of the input vector ***x*** by the corresponding element of the vector of weights ***w*** and then takes their sum and adds the bias.

***(x1 * w1 + x2 * w2 + ... + xn * wn) + bias***

***bias = 1 * w0***

    float perceptron::netInput(vector<float> X)
    {
        // Sum(Vector of weights * Input vector) + bias
        float probabilities = m_w[0]; // In this example I am adding the perceptron first
        for (int i = 0; i < X.size(); i++)
        {
            probabilities += X[i] * m_w[i + 1]; // Notice that for the weights I am counting
            // from the 2nd element since w0 is the bias and I already added it first.
        }
        return probabilities;
    }
Alright so we are now pretty much done last thing we need to do is to write the ***fit*** function which modifies the weights.

    void perceptron::fit(vector< vector<float> > X, vector<float> y)
    {
        for (int i = 0; i < X[0].size() + 1; i++) // X[0].size() + 1 -> I am using +1 to add the bias term
        {
            m_w.push_back(0); // Setting each weight to 0 and making the size of the vector
            // The same as the number of features (X[0].size()) + 1 for the bias term
        }
        for (int i = 0; i < m_epochs; i++) // Iterating through each epoch
        {
            for (int j = 0; j < X.size(); j++) // Iterating though each vector in our training Matrix
            {
                float update = m_eta * (y[j] - predict(X[j])); //we calculate the change for the weights
                for (int w = 1; w < m_w.size(); w++){ m_w[w] += update * X[j][w - 1]; } // we update each weight by the update * the training sample
                m_w[0] = update; // We update the Bias term and setting it equal to the update
            }
        }
    }

So that was essentially it. With only 3 functions we now have a working perceptron class that we can use to make predictions!

In case you want to copy-paste the code and try it out. Here is the entire class (I added some extra functionality such as printing the weights vector and the errors in each epoch as well as added the option to import/export weights.)

Here is the code:

The class header:

    class perceptron
    {
    public:
        perceptron(float eta,int epochs);
        float netInput(vector<float> X);
        int predict(vector<float> X);
        void fit(vector< vector<float> > X, vector<float> y);
        void printErrors();
        void exportWeights(string filename);
        void importWeights(string filename);
        void printWeights();
    private:
        float m_eta;
        int m_epochs;
        vector < float > m_w;
        vector < float > m_errors;
    };

The class .cpp file with the functions:

    perceptron::perceptron(float eta, int epochs)
    {
        m_epochs = epochs;
        m_eta = eta;
    }

    void perceptron::fit(vector< vector<float> > X, vector<float> y)
    {
        for (int i = 0; i < X[0].size() + 1; i++) // X[0].size() + 1 -> I am using +1 to add the bias term
        {
            m_w.push_back(0);
        }
        for (int i = 0; i < m_epochs; i++)
        {
            int errors = 0;
            for (int j = 0; j < X.size(); j++)
            {
                float update = m_eta * (y[j] - predict(X[j]));
                for (int w = 1; w < m_w.size(); w++){ m_w[w] += update * X[j][w - 1]; }
                m_w[0] = update;
                errors += update != 0 ? 1 : 0;
            }
            m_errors.push_back(errors);
        }
    }

    float perceptron::netInput(vector<float> X)
    {
        // Sum(Vector of weights * Input vector) + bias
        float probabilities = m_w[0];
        for (int i = 0; i < X.size(); i++)
        {
            probabilities += X[i] * m_w[i + 1];
        }
        return probabilities;
    }

    int perceptron::predict(vector<float> X)
    {
        return netInput(X) > 0 ? 1 : -1; //Step Function
    }

    void perceptron::printErrors()
    {
        printVector(m_errors);
    }

    void perceptron::exportWeights(string filename)
    {
        ofstream outFile;
        outFile.open(filename);

        for (int i = 0; i < m_w.size(); i++)
        {
            outFile << m_w[i] << endl;
        }

        outFile.close();
    }

    void perceptron::importWeights(string filename)
    {
        ifstream inFile;
        inFile.open(filename);

        for (int i = 0; i < m_w.size(); i++)
        {
            inFile >> m_w[i];
        }
    }

    void perceptron::printWeights()
    {
        cout << "weights: ";
        for (int i = 0; i < m_w.size(); i++)
        {
            cout << m_w[i] << " ";
        }
        cout << endl;
    }

Also if you want to try out an example here is an example I made:

main.cpp:

    #include <iostream>
    #include <vector>
    #include <algorithm>
    #include <fstream>
    #include <string>
    #include <math.h> 

    #include "MachineLearning.h"

    using namespace std;
    using namespace MachineLearning;

    vector< vector<float> > getIrisX();
    vector<float> getIrisy();

    int main()
    {
        vector< vector<float> > X = getIrisX();
        vector<float> y = getIrisy();
        vector<float> test1;
        test1.push_back(5.0);
        test1.push_back(3.3);
        test1.push_back(1.4);
        test1.push_back(0.2);

        vector<float> test2;
        test2.push_back(6.0);
        test2.push_back(2.2);
        test2.push_back(5.0);
        test2.push_back(1.5);
        //printVector(X);
        //for (int i = 0; i < y.size(); i++){ cout << y[i] << " "; }cout << endl;

        perceptron clf(0.1, 14);
        clf.fit(X, y);
        clf.printErrors();
        cout << "Now Predicting: 5.0,3.3,1.4,0.2(CorrectClass=-1,Iris-setosa) -> " << clf.predict(test1) << endl;
        cout << "Now Predicting: 6.0,2.2,5.0,1.5(CorrectClass=1,Iris-virginica) -> " << clf.predict(test2) << endl;

        system("PAUSE");
        return 0;
    }

    vector<float> getIrisy()
    {
        vector<float> y;

        ifstream inFile;
        inFile.open("y.data");
        string sampleClass;
        for (int i = 0; i < 100; i++)
        {
            inFile >> sampleClass;
            if (sampleClass == "Iris-setosa")
            {
                y.push_back(-1);
            }
            else
            {
                y.push_back(1);
            }
        }

        return y;
    }

    vector< vector<float> > getIrisX()
    {
        ifstream af;
        ifstream bf;
        ifstream cf;
        ifstream df;
        af.open("a.data");
        bf.open("b.data");
        cf.open("c.data");
        df.open("d.data");

        vector< vector<float> > X;

        for (int i = 0; i < 100; i++)
        {
            char scrap;
            int scrapN;
            af >> scrapN;
            bf >> scrapN;
            cf >> scrapN;
            df >> scrapN;

            af >> scrap;
            bf >> scrap;
            cf >> scrap;
            df >> scrap;
            float a, b, c, d;
            af >> a;
            bf >> b;
            cf >> c;
            df >> d;
            X.push_back(vector < float > {a, b, c, d});
        }

        af.close();
        bf.close();
        cf.close();
        df.close();

        return X;
    }
The way I imported the iris dataset isn't really ideal but I just wanted something that worked.

The data files can be found [here.][1]

I hope that you found this helpful!
 


  [1]: https://drive.google.com/folderview?id=0B_r3mf9HbUrLMXRhWGlTWHA3N1E&usp=sharing

## What exactly is a perceptron?
[![Visualization of a perceptron][1]][1]


At its core a perceptron model is one of the simplest [**supervised learning**][2] algorithms for [binary classification][3]. It is a type of [linear classifier][4], i.e. a classification algorithm that makes its predictions based on a linear predictor function combining a set of weights with the feature vector.
A more intuitive way to think about is like a **Neural Network with only one neuron**.

The way it works is very simple. It gets a vector of input values ***x*** of which each element is a feature of our data set. 

An Example:
===========

Say that we want to classify whether an object is a bicycle or a car. For the sake of this example let's say that we want to select 2 features. The height and the width of the object. In that case ***x = [x1, x2]*** where ***x1*** is the height and ***x2*** is the width.

Then once we have our input vector ***x*** we want to multiply each element in that vector with a weight. Usually the higher the value of the weight the more important the feature is. If for example we used ***color*** as feature ***x3*** and there is a red bike and a red car the perceptron will set a very low weight to it so that the color doesn't impact the final prediction.

Alright so we have multiplied the 2 vectors ***x*** and ***w*** and we got back a vector. Now we need to sum the elements of this vector. A smart way to do this is instead of simple multiplying ***x*** by ***w*** we can multiply ***x*** by ***wT*** where ***T*** stands for transpose. We can imagine the ***transpose*** of a vector as a rotated version of the vector. For more info you can read [the Wikipedia page][5]. Essentially by taking the transpose of the vector ***w*** we get an ***Nx1*** vector instead of a ***1xN***. Thus if we now multiply our input vector with size ***1xN*** with this ***Nx1*** weight vector we will get a ***1x1*** vector (or simply a single value) which will be equal to ***x1 * w1 + x2 * w2 + ... + xn * wn***. Having done that, we now have our prediction. But there is one last thing. This prediction will probably not be a simple 1 or -1 to be able to classify a new sample. So what we can do is to simply say the following: If our prediction is bigger than 0 then we say that the sample belongs to class 1, otherwise if the prediction is smaller than zero we say that the sample belongs to the class -1. This is called a [step function][6].

But how do we get the right weights so that we do correct predictions? In other words, how do we ***train*** our perceptron model?

Well in the case of the perceptron we do not need fancy math equations to ***train*** our model. Our weights can be adjusted by the following equation:

***Δw = eta * (y - prediction) * x(i)*** 

where ***x(i)*** is our feature (x1 for example for weight 1, x2 for w2 and so on...).

Also noticed that there is a variable called ***eta*** that is the learning rate. You can imagine the learning rate as how big we want the change of the weights to be. A good learning rate results in a fast learning algorithm. A too high value of ***eta*** can result in an increasing amount of errors at each epoch and results in the model doing really bad predictions and never converging. Too low of a learning rate can have as a result the model to take too much time to converge. (Usually a good value to set ***eta*** to for the perceptron model is 0.1 but it can differ from case to case). 

Finally some of you might have noticed that the first input is a constant ( 1 ) and is multiplied by w0. So what exactly is that? In order to get a good prediction we need to add a bias. And that's exactly what that constant is.

To modify the weight of the bias term we use the same equation as we did for the other weights but in this case we do not multiply it by the input (because the input is a constant 1 and so we don't have to):

***Δw = eta * (y - prediction)*** 

So that is basically how a simple perceptron model works! Once we train our weights we can give it new data and have our predictions.

NOTE:
=====

The Perceptron model has an important disadvantage! It will never converge (e.i find the perfect weights) if the data isn't [linearly separable][7], which means being able to separate the 2 classes in a feature space by a straight line. So in order to avoid that it is a good practice to add a fixed number of iterations so that the model isn't stuck at adjusting weights that will never be perfectly tuned.


  [1]: http://i.stack.imgur.com/1tsTd.png
  [2]: https://www.wikiod.com/machine-learning/supervised-learning
  [3]: https://en.wikipedia.org/wiki/Binary_classification
  [4]: https://en.wikipedia.org/wiki/Linear_classifier
  [5]: https://en.wikipedia.org/wiki/Transpose
  [6]: https://en.wikipedia.org/wiki/Step_function
  [7]: https://en.wikipedia.org/wiki/Linear_separability

## What is the bias
----------


What is the bias
----------------

A perceptron can be seen as a function that maps an input (real-valued) vector ***x*** to an output value ***f(x)*** (binary value):

[![enter image description here][1]][1]

where ***w*** is a vector of real-valued weights and ***b*** is a our *bias* value.
The bias is a value that shifts the decision boundary away from the origin *(0,0)* and that does not depend on any input value.

Thinking at the bias in a spatial way, the bias alters the position (though not the orientation) of the decision boundary.
We can see below an example of the same curve shifted by the bias:

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/6i1bF.png
  [2]: https://i.stack.imgur.com/EYgT3.jpg

