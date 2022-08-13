---
title: "Dimensionality reduction (Feature selection)"
slug: "dimensionality-reduction-feature-selection"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Reducing The Dimension With Principal Component Analysis
[Principal Component Analysis](https://en.wikipedia.org/wiki/Dimensionality_reduction#Principal_component_analysis) finds sequences of linear combinations of the features. The first linear combination maximizes the variance of the features (subject to a unit constraint). Each of the following linear combinations maximizes the variance of the features in the subspace orthogonal to that spanned by the previous linear combinations. 

A common dimension reduction technique is to use only the *k* first such linear combinations. Suppose the features are a matrix *X* of *n* rows and *m* columns. The first *k* linear combinations form a matrix *&beta;<sub>k</sub>* of *m* rows and *k* columns. The product *X &beta;* has *n* rows and *k* columns. Thus, the resulting matrix *&beta; k* can be considered a reduction from *m* to *k* dimensions, retaining the high-variance parts of the original matrix *X*.

In `scikit-learn`, PCA is performed with [`sklearn.decomposition.PCA`](http://scikit-learn.org/stable/modules/generated/sklearn.decomposition.PCA.html#sklearn.decomposition.PCA). For example, suppose we start with a 100 X 7 matrix, constructed so that the variance is contained only in the first two columns (by scaling down the last 5 columns):

    import numpy as np
    np.random.seed(123) # we'll set a random seed so that our results are reproducible
    X = np.hstack((np.random.randn(100, 2) + (10, 10), 0.001 * np.random.randn(100, 5)))

Let's perform a reduction to 2 dimensions:

    from sklearn.decomposition import PCA
    pca = PCA(n_components=2)
    pca.fit(X)

Now let's check the results. First, here are the linear combinations: 

    pca.components_
    # array([[ -2.84271217e-01,  -9.58743893e-01,  -8.25412629e-05,
    #           1.96237855e-05,  -1.25862328e-05,   8.27127496e-05,
    #          -9.46906600e-05],
    #        [ -9.58743890e-01,   2.84271223e-01,  -7.33055823e-05,
    #          -1.23188872e-04,  -1.82458739e-05,   5.50383246e-05,
    #           1.96503690e-05]])

Note how the first two components in each vector are several orders of magnitude larger than the others, showing that the PCA recognized that the variance is contained mainly in the first two columns.

To check the ratio of the variance explained by this PCA, we can examine `pca.explained_variance_ratio_`:

    pca.explained_variance_ratio_
    # array([ 0.57039059,  0.42960728])

