---
title: "Model selection"
slug: "model-selection"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Cross-validation
Learning the parameters of a prediction function and testing it on the same data is a methodological mistake: a model that would just repeat the labels of the samples that it has just seen would have a perfect score but would fail to predict anything useful on yet-unseen data. This situation is called **overfitting**. To avoid it, it is common practice when performing a (supervised) machine learning experiment to hold out part of the available data as a **test set** `X_test, y_test`. Note that the word “experiment” is not intended to denote academic use only, because even in commercial settings machine learning usually starts out experimentally.

In [scikit-learn][1] a random split into training and test sets can be quickly computed with the [train_test_split][2] helper function. Let’s load the iris data set to fit a linear support vector machine on it:

    >>> import numpy as np
    >>> from sklearn import cross_validation
    >>> from sklearn import datasets
    >>> from sklearn import svm
    
    >>> iris = datasets.load_iris()
    >>> iris.data.shape, iris.target.shape
    ((150, 4), (150,))

We can now quickly sample a training set while holding out 40% of the data for testing (evaluating) our classifier:

    >>> X_train, X_test, y_train, y_test = cross_validation.train_test_split(
    ...     iris.data, iris.target, test_size=0.4, random_state=0)
    
    >>> X_train.shape, y_train.shape
    ((90, 4), (90,))
    >>> X_test.shape, y_test.shape
    ((60, 4), (60,))
    
Now, after we have train and test sets, lets use it:

    >>> clf = svm.SVC(kernel='linear', C=1).fit(X_train, y_train)
    >>> clf.score(X_test, y_test)   


  [1]: http://scikit-learn.org/stable/index.html
  [2]: http://scikit-learn.org/stable/modules/generated/sklearn.cross_validation.train_test_split.html#sklearn.cross_validation.train_test_split

## K-Fold Cross Validation
K-fold cross-validation is a systematic process for repeating the train/test split procedure multiple times, in order to reduce the variance associated with a single trial of train/test split. You essentially split the entire dataset into K equal size "folds", and each fold is used once for testing the model and K-1 times for training the model. 

Multiple folding techniques are available with the scikit library. Their usage is dependent on the input data characteristics. Some examples are 

# K-Fold
You essentially split the entire dataset into K equal size "folds", and each fold is used once for testing the model and K-1 times for training the model.

    from sklearn.model_selection import KFold
    X = np.array([[1, 2], [3, 4], [5, 6], [7, 8]])
    y = np.array([1, 2, 1, 2])
    cv = KFold(n_splits=3, random_state=0)
    
    for train_index, test_index in cv.split(X):
    ...    print("TRAIN:", train_index, "TEST:", test_index)

    TRAIN: [2 3] TEST: [0 1]
    TRAIN: [0 1 3] TEST: [2]
    TRAIN: [0 1 2] TEST: [3]

 
`StratifiedKFold` is a variation of k-fold which returns stratified folds: each set contains approximately the same percentage of samples of each target class as the complete set 

# ShuffleSplit
Used to generate a user defined number of independent train / test dataset splits. Samples are first shuffled and then split into a pair of train and test sets.

    from sklearn.model_selection import ShuffleSplit
    X = np.array([[1, 2], [3, 4], [5, 6], [7, 8]])
    y = np.array([1, 2, 1, 2])
    cv = ShuffleSplit(n_splits=3, test_size=.25, random_state=0)
    
    for train_index, test_index in cv.split(X):
    ...    print("TRAIN:", train_index, "TEST:", test_index)

    TRAIN: [3 1 0] TEST: [2]
    TRAIN: [2 1 3] TEST: [0]
    TRAIN: [0 2 1] TEST: [3]



`StratifiedShuffleSplit` is a variation of ShuffleSplit, which returns stratified splits, i.e which creates splits by preserving the same percentage for each target class as in the complete set.

Other folding techniques such as Leave One/p Out, and TimeSeriesSplit (a variation of K-fold) are available in the scikit model_selection library.


