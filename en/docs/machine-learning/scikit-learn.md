---
title: "Scikit Learn"
slug: "scikit-learn"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## A Basic Simple Classification Problem(XOR) using k nearest neighbor algorithm
Consider you want to predict the correct answer for XOR popular problem. You Knew what is XOR(e.g [x0 x1] => y). for example [0 0] => 0, [0 1] => [1] and...

    #Load Sickit learn data
    from sklearn.neighbors import KNeighborsClassifier

    #X is feature vectors, and y is correct label(To train model)
    X = [[0, 0],[0 ,1],[1, 0],[1, 1]]
    y = [0,1,1,0]

    #Initialize a Kneighbors Classifier with K parameter set to 2
    KNC = KNeighborsClassifier(n_neighbors= 2)

    #Fit the model(the KNC learn y Given X)
    KNC.fit(X, y)
    
    #print the predicted result for [1 1]
    print(KNC.predict([[1 1]]))



## Classification in scikit-learn
**1. Bagged Decision Trees**

Bagging performs best with algorithms that have high variance. A popular example are decision trees, often constructed without pruning.

In the example below see an example of using the BaggingClassifier with the Classification and Regression Trees algorithm (DecisionTreeClassifier). A total of 100 trees are created.

Dataset Used: [Pima Indians Diabetes Data Set][1]

  

    # Bagged Decision Trees for Classification
    import pandas
    from sklearn import cross_validation
    from sklearn.ensemble import BaggingClassifier
    from sklearn.tree import DecisionTreeClassifier
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
    names = ['preg', 'plas', 'pres', 'skin', 'test', 'mass', 'pedi', 'age', 'class']
    dataframe = pandas.read_csv(url, names=names)
    array = dataframe.values
    X = array[:,0:8]
    Y = array[:,8]
    num_folds = 10
    num_instances = len(X)
    seed = 7
    kfold = cross_validation.KFold(n=num_instances, n_folds=num_folds, random_state=seed)
    cart = DecisionTreeClassifier()
    num_trees = 100
    model = BaggingClassifier(base_estimator=cart, n_estimators=num_trees, random_state=seed)
    results = cross_validation.cross_val_score(model, X, Y, cv=kfold)
    print(results.mean())

Running the example, we get a robust estimate of model accuracy.

     
    0.770745044429


**2. Random Forest**

Random forest is an extension of bagged decision trees.

Samples of the training dataset are taken with replacement, but the trees are constructed in a way that reduces the correlation between individual classifiers. Specifically, rather than greedily choosing the best split point in the construction of the tree, only a random subset of features are considered for each split.

You can construct a Random Forest model for classification using the RandomForestClassifier class.

The example below provides an example of Random Forest for classification with 100 trees and split points chosen from a random selection of 3 features.

    # Random Forest Classification
    import pandas
    from sklearn import cross_validation
    from sklearn.ensemble import RandomForestClassifier
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
    names = ['preg', 'plas', 'pres', 'skin', 'test', 'mass', 'pedi', 'age', 'class']
    dataframe = pandas.read_csv(url, names=names)
    array = dataframe.values
    X = array[:,0:8]
    Y = array[:,8]
    num_folds = 10
    num_instances = len(X)
    seed = 7
    num_trees = 100
    max_features = 3
    kfold = cross_validation.KFold(n=num_instances, n_folds=num_folds, random_state=seed)
    model = RandomForestClassifier(n_estimators=num_trees, max_features=max_features)
    results = cross_validation.cross_val_score(model, X, Y, cv=kfold)
    print(results.mean())

Running the example provides a mean estimate of classification accuracy.

    0.770727956254

**3. AdaBoost**

AdaBoost was perhaps the first successful boosting ensemble algorithm. It generally works by weighting instances in the dataset by how easy or difficult they are to classify, allowing the algorithm to pay or or less attention to them in the construction of subsequent models.

You can construct an AdaBoost model for classification using the AdaBoostClassifier class.

The example below demonstrates the construction of 30 decision trees in sequence using the AdaBoost algorithm.

    # AdaBoost Classification
    import pandas
    from sklearn import cross_validation
    from sklearn.ensemble import AdaBoostClassifier
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
    names = ['preg', 'plas', 'pres', 'skin', 'test', 'mass', 'pedi', 'age', 'class']
    dataframe = pandas.read_csv(url, names=names)
    array = dataframe.values
    X = array[:,0:8]
    Y = array[:,8]
    num_folds = 10
    num_instances = len(X)
    seed = 7
    num_trees = 30
    kfold = cross_validation.KFold(n=num_instances, n_folds=num_folds, random_state=seed)
    model = AdaBoostClassifier(n_estimators=num_trees, random_state=seed)
    results = cross_validation.cross_val_score(model, X, Y, cv=kfold)
    print(results.mean())

Running the example provides a mean estimate of classification accuracy.

    0.76045796309


**4. Stochastic Gradient Boosting**

Stochastic Gradient Boosting (also called Gradient Boosting Machines) are one of the most sophisticated ensemble techniques. It is also a technique that is proving to be perhaps of the best techniques available for improving performance via ensembles.

You can construct a Gradient Boosting model for classification using the GradientBoostingClassifier class.

The example below demonstrates Stochastic Gradient Boosting for classification with 100 trees.

    # Stochastic Gradient Boosting Classification
    import pandas
    from sklearn import cross_validation
    from sklearn.ensemble import GradientBoostingClassifier
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
    names = ['preg', 'plas', 'pres', 'skin', 'test', 'mass', 'pedi', 'age', 'class']
    dataframe = pandas.read_csv(url, names=names)
    array = dataframe.values
    X = array[:,0:8]
    Y = array[:,8]
    num_folds = 10
    num_instances = len(X)
    seed = 7
    num_trees = 100
    kfold = cross_validation.KFold(n=num_instances, n_folds=num_folds, random_state=seed)
    model = GradientBoostingClassifier(n_estimators=num_trees, random_state=seed)
    results = cross_validation.cross_val_score(model, X, Y, cv=kfold)
    print(results.mean())

Running the example provides a mean estimate of classification accuracy.

    0.764285714286


  [1]: https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes

Source: http://machinelearningmastery.com/ensemble-machine-learning-algorithms-python-scikit-learn/



