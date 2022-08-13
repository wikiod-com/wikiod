---
title: "Getting started with scikit-learn"
slug: "getting-started-with-scikit-learn"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of scikit-learn
The current stable version of scikit-learn [requires][3]:
   - Python (>= 2.6 or >= 3.3),
   - NumPy (>= 1.6.1),
   - SciPy (>= 0.9).


----------


For most installation `pip` python package manager can install python and all of its dependencies:
```
pip install scikit-learn
```

However for linux systems it is recommended to use `conda` package manager to avoid possible build processes
```
conda install scikit-learn
```

To check that you have `scikit-learn`, execute in shell:
```
python -c 'import sklearn; print(sklearn.__version__)'
```


----------

**Windows and Mac OSX Installation:**

[Canopy][1] and [Anaconda][2] both ship a recent version of *scikit-learn*, in addition to a large set of scientific python library for Windows, Mac OSX (also relevant for Linux).


  [1]: https://www.enthought.com/products/canopy/
  [2]: https://www.continuum.io/downloads
  [3]: http://scikit-learn.org/stable/install.html

## Creating pipelines
Finding patterns in data often proceeds in a chain of data-processing steps, e.g., feature selection, normalization, and classification. In `sklearn`, a pipeline of stages is used for this. 

For example, the following code shows a pipeline consisting of two stages. The first scales the features, and the second trains a classifier on the resulting augmented dataset:

    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler
    from sklearn.neighbors import KNeighborsClassifier

    pipeline = make_pipeline(StandardScaler(), KNeighborsClassifier(n_neighbors=4))

Once the pipeline is created, you can use it like a regular stage (depending on its specific steps). Here, for example, the pipeline behaves like a classifier. Consequently, we can use it as follows:

    # fitting a classifier
    pipeline.fit(X_train, y_train)
    # getting predictions for the new data sample
    pipeline.predict_proba(X_test)


## Train a classifier with cross-validation
Using iris dataset:
```
import sklearn.datasets
iris_dataset = sklearn.datasets.load_iris()
X, y = iris_dataset['data'], iris_dataset['target']
```

Data is split into train and test sets. To do this we use the `train_test_split` utility function to split both `X` and `y` (data and target vectors) randomly with the option `train_size=0.75` (training sets contain 75% of the data). 

Training datasets are fed into a [k-nearest neighbors classifier][1]. The method `fit` of the classifier will fit the model to the data.

```
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, train_size=0.75) 
from sklearn.neighbors import KNeighborsClassifier
clf = KNeighborsClassifier(n_neighbors=3)
clf.fit(X_train, y_train)
```
Finally predicting quality on test sample:
```
clf.score(X_test, y_test) # Output: 0.94736842105263153
```

By using one pair of train and test sets we might get a biased estimation of the quality of the classifier due to the arbitrary choice the data split. 
By using _cross-validation_ we can fit of the classifier on different train/test subsets of the data and make an average over all accuracy results. 
The function `cross_val_score` fits a classifier to the input data using cross-validation. It can take as input the number of different splits (folds) to be used (5 in the example below).

````
from sklearn.cross_validation import cross_val_score
scores = cross_val_score(clf, X, y, cv=5)
print(scores)
# Output: array([ 0.96666667,  0.96666667,  0.93333333,  0.96666667,  1.        ])
print "Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() / 2)
# Output: Accuracy: 0.97 (+/- 0.03)
````

  [1]: https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm



## Sample datasets
For ease of testing, `sklearn` provides some built-in datasets in `sklearn.datasets` module. For example, let's load Fisher's iris dataset:
```
import sklearn.datasets
iris_dataset = sklearn.datasets.load_iris()
iris_dataset.keys()
['target_names', 'data', 'target', 'DESCR', 'feature_names']
```
You can read full description, names of features and names of classes (`target_names`). Those are stored as strings. 

We are interested in the data and classes, which stored in `data` and `target` fields. By convention those are denoted as `X` and `y`
```
X, y = iris_dataset['data'], iris_dataset['target']
X.shape, y.shape
((150, 4), (150,))
```
```
numpy.unique(y)
array([0, 1, 2])
```

Shapes of `X` and `y` say that there are 150 samples with 4 features. Each sample belongs to one of following classes: 0, 1 or 2.

`X` and `y` can now be used in training a classifier, by calling the classifier's `fit()` method.

----

Here is the full list of datasets provided by the `sklearn.datasets` module with their size and intended use:

| Load with| Description| Size |Usage
| ------ | ------ | ------ |------ |
|`load_boston()` |Boston house-prices dataset |506|regression
|`load_breast_cancer()`  |Breast cancer Wisconsin dataset |569|classification (binary)
|`load_diabetes()` |Diabetes dataset |442|regression
|`load_digits(n_class)` |Digits dataset |1797|classification
|`load_iris()` |Iris dataset | 150| classification (multi-class)
|`load_linnerud()` |Linnerud dataset |20|multivariate regression

Note that (source: http://scikit-learn.org/stable/datasets/):

> These datasets are useful to quickly illustrate the behavior of the
> various algorithms implemented in the scikit. They are however often
> too small to be representative of real world machine learning tasks.

In addition to these built-in toy sample datasets, `sklearn.datasets` also provides utility functions for loading external datasets:

- `load_mlcomp` for loading sample datasets from the [mlcomp.org][1] repository (note that the datasets need to be downloaded before). [Here](http://scikit-learn.org/stable/auto_examples/text/mlcomp_sparse_document_classification.html) is an example of usage.
- `fetch_lfw_pairs` and `fetch_lfw_people` for loading Labeled Faces in the Wild (LFW) pairs dataset from http://vis-www.cs.umass.edu/lfw/, used for face verification (resp. face recognition). This dataset is larger than 200 MB. [Here](http://scikit-learn.org/stable/datasets/labeled_faces.html) is an example of usage.


  [1]: http://mlcomp.org

## Interfaces and conventions: 
Different operations with data are done using special classes.

Most of the classes belong to one of the following groups:
* classification algorithms (derived from `sklearn.base.ClassifierMixin`) to solve classification problems
* regression algorithms (derived from `sklearn.base.RegressorMixin`) to solve problem of reconstructing continuous variables (regression problem)
* data transformations (derived from `sklearn.base.TransformerMixin`) that preprocess the data


Data is stored in `numpy.array`s (but other array-like objects like `pandas.DataFrame`s are accepted if those are convertible to `numpy.array`s)

Each object in the data is described by set of features the general convention is that data sample is represented with array, where first dimension is data sample id, second dimension is feature id. 

```
import numpy
data = numpy.arange(10).reshape(5, 2)
print(data)

Output:
[[0 1]
 [2 3]
 [4 5]
 [6 7]
 [8 9]]
```

In `sklearn` conventions dataset above contains 5 objects each described by 2 features.


