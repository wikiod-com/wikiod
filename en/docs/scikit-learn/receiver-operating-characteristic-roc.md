---
title: "Receiver Operating Characteristic (ROC)"
slug: "receiver-operating-characteristic-roc"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Introduction to ROC and AUC
Example of Receiver Operating Characteristic (ROC) metric to evaluate classifier output quality.

ROC curves typically feature true positive rate on the Y axis, and false positive rate on the X axis. This means that the top left corner of the plot is the “ideal” point - a false positive rate of zero, and a true positive rate of one. This is not very realistic, but it does mean that a larger area under the curve (AUC) is usually better.

The “steepness” of ROC curves is also important, since it is ideal to maximize the true positive rate while minimizing the false positive rate.

A simple example:

    import numpy as np
    from sklearn import metrics
    import matplotlib.pyplot as plt
    
Arbitrary `y` values - in real case this is the predicted target values (`model.predict(x_test)` ): 

    y = np.array([1,1,2,2,3,3,4,4,2,3])

Scores is the mean accuracy on the given test data and labels (`model.score(X,Y)`):

    scores = np.array([0.3, 0.4, 0.95,0.78,0.8,0.64,0.86,0.81,0.9, 0.8])

Calculate the ROC curve and the AUC:

    fpr, tpr, thresholds = metrics.roc_curve(y, scores, pos_label=2)
    roc_auc = metrics.auc(fpr, tpr)
    
Plotting:

    plt.figure()
    plt.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc)
    plt.plot([0, 1], [0, 1], 'k--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic example')
    plt.legend(loc="lower right")
    plt.show()

Output:

[![enter image description here][1]][1]

Note: the sources were taken from these [link1][2] and [link2][3]


  [1]: http://i.stack.imgur.com/CYShh.jpg
  [2]: http://scikit-learn.org/stable/auto_examples/model_selection/plot_roc.html
  [3]: http://scikit-learn.org/stable/modules/generated/sklearn.metrics.roc_curve.html

## ROC-AUC score with overriding and cross validation
One needs the predicted probabilities in order to calculate the ROC-AUC (area under the curve) score. The `cross_val_predict` uses the `predict` methods of classifiers. In order to be able to get the ROC-AUC score, one can simply subclass the classifier, overriding the `predict` method, so that it would act like `predict_proba`. 
```python
from sklearn.datasets import make_classification
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import cross_val_predict
from sklearn.metrics import roc_auc_score

class LogisticRegressionWrapper(LogisticRegression):
    def predict(self, X):
        return super(LogisticRegressionWrapper, self).predict_proba(X)

X, y = make_classification(n_samples = 1000, n_features=10, n_classes = 2, flip_y = 0.5)

log_reg_clf = LogisticRegressionWrapper(C=0.1, class_weight=None, dual=False,
             fit_intercept=True)

y_hat = cross_val_predict(log_reg_clf, X, y)[:,1]

print("ROC-AUC score: {}".format(roc_auc_score(y, y_hat)))
```
output:
```
ROC-AUC score: 0.724972396025
```


