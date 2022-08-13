---
title: "Classification"
slug: "classification"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## RandomForestClassifier
A random forest is a meta estimator that fits a number of decision tree classifiers on various sub-samples of the dataset and use averaging to improve the predictive accuracy and control over-fitting.

A simple usage example:

Import:

    from sklearn.ensemble import RandomForestClassifier

Define train data and target data:

    train = [[1,2,3],[2,5,1],[2,1,7]]
    target = [0,1,0]

The values in `target` represent the label you want to predict.

Initiate a RandomForest object and perform learn (fit):

    rf = RandomForestClassifier(n_estimators=100)
    rf.fit(train, target)

Predict:

    test = [2,2,3]
    predicted = rf.predict(test)



## Analyzing Classification Reports
Build a text report showing the main classification metrics, including the [precision and recall][1], [f1-score][2] (the [harmonic mean][3] of precision and recall) and support (the number of observations of that class in the training set).

Example from `sklearn` [docs][4]: 

    from sklearn.metrics import classification_report
    y_true = [0, 1, 2, 2, 2]
    y_pred = [0, 0, 2, 2, 1]
    target_names = ['class 0', 'class 1', 'class 2']
    print(classification_report(y_true, y_pred, target_names=target_names))

Output - 

             precision    recall  f1-score   support

    class 0       0.50      1.00      0.67         1
    class 1       0.00      0.00      0.00         1
    class 2       1.00      0.67      0.80         3

    avg / total   0.70      0.60      0.61         5


  [1]: https://en.wikipedia.org/wiki/Precision_and_recall
  [2]: https://en.wikipedia.org/wiki/F1_score
  [3]: https://en.wikipedia.org/wiki/Harmonic_mean
  [4]: http://scikit-learn.org/stable/modules/generated/sklearn.metrics.classification_report.html

## GradientBoostingClassifier
[Gradient Boosting][1] for classification. The Gradient Boosting Classifier is an additive ensemble of a base model whose error is corrected in successive iterations (or stages) by the addition of Regression Trees which correct the residuals (the error of the previous stage).

**Import:**

    from sklearn.ensemble import GradientBoostingClassifier

**Create some toy classification data**
    
    from sklearn.datasets import load_iris

    iris_dataset = load_iris()

    X, y = iris_dataset.data, iris_dataset.target

**Let us split this data into training and testing set.**

    from sklearn.model_selection import train_test_split
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=0)

**Instantiate a `GradientBoostingClassifier` model using the default params.**

    gbc = GradientBoostingClassifier()
    gbc.fit(X_train, y_train)

**Let us score it on the test set**

    # We are using the default classification accuracy score
    >>> gbc.score(X_test, y_test)
    1

**By default there are 100 estimators built**

    >>> gbc.n_estimators
    100

This can be controlled by setting `n_estimators` to a different value during the initialization time.





    


  [1]: http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingClassifier.html

## Using Support Vector Machines
[Support vector machines](https://en.wikipedia.org/wiki/Support_vector_machine) is a family of algorithms attempting to pass a (possibly high-dimension) hyperplane between two labelled sets of points, such that the distance of the points from the plane is optimal in some sense. SVMs can be used for classification or regression (corresponding to [`sklearn.svm.SVC`](http://scikit-learn.org/stable/modules/generated/sklearn.svm.SVC.html) and [`sklearn.svm.SVR`](http://scikit-learn.org/stable/modules/generated/sklearn.svm.SVR.html), respectively.
            
**Example:**

Suppose we work in a 2D space. First, we create some data:

    import numpy as np

Now we create *x* and *y*:

    x0, x1 = np.random.randn(10, 2), np.random.randn(10, 2) + (1, 1)
    x = np.vstack((x0, x1))

    y = [0] * 10 + [1] * 10

Note that *x* is composed of two Gaussians: one centered around *(0, 0)*, and one centered around *(1, 1)*.

To build a classifier, we can use:

    from sklearn import svm

    svm.SVC(kernel='linear').fit(x, y)

Let's check the prediction for *(0, 0)*:

    >>> svm.SVC(kernel='linear').fit(x, y).predict([[0, 0]])
    array([0])

The prediction is that the class is 0.

For regression, we can similarly do:

    svm.SVR(kernel='linear').fit(x, y)

## A Decision Tree
A decision tree is a classifier which uses a sequence of verbose rules (like a>7) which can be easily understood. 

The example below trains a decision tree classifier using three feature vectors of length 3, and then predicts the result for a so far unknown fourth feature vector, the so called test vector.

    from sklearn.tree import DecisionTreeClassifier
    
    # Define training and target set for the classifier
    train = [[1,2,3],[2,5,1],[2,1,7]]
    target = [10,20,30]
    
    # Initialize Classifier. 
    # Random values are initialized with always the same random seed of value 0 
    # (allows reproducible results)
    dectree = DecisionTreeClassifier(random_state=0)
    dectree.fit(train, target)
    
    # Test classifier with other, unknown feature vector
    test = [2,2,3]
    predicted = dectree.predict(test)
    
    print predicted

Output can be visualized using:

    import pydot
    import StringIO
    
    dotfile = StringIO.StringIO()
    tree.export_graphviz(dectree, out_file=dotfile)
    (graph,)=pydot.graph_from_dot_data(dotfile.getvalue())
    graph.write_png("dtree.png")
    graph.write_pdf("dtree.pdf")



## Classification using Logistic Regression
In LR Classifier, he probabilities describing the possible outcomes of a single trial are modeled using a logistic function. It is implemented in the `linear_model` library

    from sklearn.linear_model import LogisticRegression

The sklearn LR implementation can fit binary, One-vs- Rest, or multinomial logistic regression with optional L2 or L1 regularization. For example, let us consider a binary classification on a sample sklearn dataset

    from sklearn.datasets import make_hastie_10_2

    X,y = make_hastie_10_2(n_samples=1000)

Where X is a `n_samples X 10` array and y is the target labels -1 or +1.

Use train-test split to divide the input data into training and test sets (70%-30%)

    from sklearn.model_selection import train_test_split 
    #sklearn.cross_validation in older scikit versions
    
    data_train, data_test, labels_train, labels_test = train_test_split(X,y, test_size=0.3)

Using the LR Classifier is similar to other examples

    # Initialize Classifier. 
    LRC = LogisticRegression()
    LRC.fit(data_train, labels_train)
    
    # Test classifier with the test data
    predicted = LRC.predict(data_test)

Use Confusion matrix to visualise results

    from sklearn.metrics import confusion_matrix
    
    confusion_matrix(predicted, labels_test)

