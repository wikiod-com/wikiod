---
title: "SVM"
slug: "svm"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Difference between logistic regression and SVM
Decision boundary when we classify using **logistic regression**-
[![Logistic Regression][1]][1]

Decision boundary when we classify using **SVM**-

[![Classification using SVM][2]][2]

As it can be observed, SVM tries to maintain a 'gap' on either side of the decision boundary. This proves helpful when we encounter new data.


**With new data-**

Logistic regression performs **poorly** (new red circle is classified as blue) -

[![New data (red circle) with logistic regression's decision boundary][3]][3]
  
Whereas **SVM** can classify it correctly (the new red circle is classified correctly in red side)-

[![New red circle is classified correctly in SVM][4]][4]


  [1]: http://i.stack.imgur.com/xhCjE.png
  [2]: http://i.stack.imgur.com/f8dTL.png
  [3]: http://i.stack.imgur.com/seiON.png
  [4]: http://i.stack.imgur.com/lis2U.png

## Implementing SVM classifier using Scikit-learn:
<!-- language: lang-python -->
    from sklearn import svm
    X = [[1, 2], [3, 4]] #Training Samples
    y = [1, 2] #Class labels
    model = svm.SVC() #Making a support vector classifier model
    model.fit(X, y) #Fitting the data

    clf.predict([[2, 3]]) #After fitting, new data can be classified by using predict()



