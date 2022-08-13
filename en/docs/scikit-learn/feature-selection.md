---
title: "Feature selection"
slug: "feature-selection"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Low-Variance Feature Removal
This is a very basic feature selection technique. 

Its underlying idea is that if a feature is constant (i.e. it has 0 variance), then it cannot be used for finding any interesting patterns and can be removed from the dataset.

Consequently, a heuristic approach to feature elimination is to first remove all features whose variance is below some (low) threshold.    

Building off the [example in the documentation](http://scikit-learn.org/stable/modules/feature_selection.html), suppose we start with    

    X = [[0, 0, 1], [0, 1, 0], [1, 0, 0], [0, 1, 1], [0, 1, 0], [0, 1, 1]]

There are *3* boolean features here, each with *6* instances. Suppose we wish to remove those that are constant in at least 80% of the instances. Some probability calculations show that these features will need to have variance lower than *0.8 * (1 - 0.8)*. Consequently, we can use

    from sklearn.feature_selection import VarianceThreshold
    sel = VarianceThreshold(threshold=(.8 * (1 - .8)))    
    sel.fit_transform(X)
    # Output: array([[0, 1],
                     [1, 0],
                     [0, 0],
                     [1, 1],
                     [1, 0],
                     [1, 1]])

Note how the first feature was removed.

This method should be used with caution because a low variance doesn't necessarily mean that a feature is “uninteresting”. Consider the following example where we construct a dataset that contains *3* features, the first two consisting of randomly distributed variables and the third of uniformly distributed variables.
```
from sklearn.feature_selection import VarianceThreshold
import numpy as np

# generate dataset
np.random.seed(0)

feat1 = np.random.normal(loc=0, scale=.1, size=100) # normal dist. with mean=0 and std=.1
feat2 = np.random.normal(loc=0, scale=10, size=100) # normal dist. with mean=0 and std=10
feat3 = np.random.uniform(low=0, high=10, size=100) # uniform dist. in the interval [0,10)
data = np.column_stack((feat1,feat2,feat3))

data[:5]
# Output:
# array([[  0.17640523,  18.83150697,   9.61936379],
#        [  0.04001572, -13.47759061,   2.92147527],
#        [  0.0978738 , -12.70484998,   2.4082878 ],
#        [  0.22408932,   9.69396708,   1.00293942],
#        [  0.1867558 , -11.73123405,   0.1642963 ]]) 

np.var(data, axis=0)
# Output: array([  1.01582662e-02,   1.07053580e+02,   9.07187722e+00])

sel = VarianceThreshold(threshold=0.1)
sel.fit_transform(data)[:5]
# Output:
# array([[ 18.83150697,   9.61936379],
#        [-13.47759061,   2.92147527],
#        [-12.70484998,   2.4082878 ],
#        [  9.69396708,   1.00293942],
#        [-11.73123405,   0.1642963 ]])
```
Now the first feature has been removed because of its low variance, while the third feature (that's the most uninteresting) has been kept. In this case it would have been more appropriate to consider a _coefficient of variation_ because that's independent of scaling.

