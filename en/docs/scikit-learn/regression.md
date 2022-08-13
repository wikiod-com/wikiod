---
title: "Regression"
slug: "regression"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Ordinary Least Squares
[Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares) is 
a method for finding the linear combination of features that best fits the observed outcome in the following sense.

If the vector of outcomes to be predicted is *y*, and the explanatory variables form the matrix *X*, then OLS will find the vector *&beta;* solving

*min<sub>&beta;</sub>|y^ - y|<sub>2</sub><sup>2</sup>*,

where *y^ = X &beta;* is the linear prediction.

In sklearn, this is done using [`sklearn.linear_model.LinearRegression`](http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html#sklearn.linear_model.LinearRegression).

**Application Context**

OLS should only be applied to regression problems, it is generally unsuitable for classification problems: Contrast

 - Is an email spam? (Classfication) 
 - What is the linear relationship
   between upvotes depend on the length of answer? (Regression)

**Example**

Let's generate a linear model with some noise, then see if `LinearRegression` Manages to reconstruct the linear model.

First we generate the `X` matrix:

    import numpy as np
 
    X = np.random.randn(100, 3)

Now we'll generate the `y` as a linear combination of `X` with some noise:

    beta = np.array([[1, 1, 0]])
    y = (np.dot(x, beta.T) + 0.01 * np.random.randn(100, 1))[:, 0]

Note that  the true linear combination generating `y` is given by `beta.

To try to reconstruct this from `X` and `y` alone, let's do:

    >>> linear_model.LinearRegression().fit(x, y).coef_
    array([  9.97768469e-01,   9.98237634e-01,   7.55016533e-04])

Note that this vector is very similar to `beta`.


