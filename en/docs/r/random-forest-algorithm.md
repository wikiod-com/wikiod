---
title: "Random Forest Algorithm"
slug: "random-forest-algorithm"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

RandomForest is an ensemble method for classification or regression that reduces the chance of overfitting the data. Details of the method can be found in the [Wikipedia article on Random Forests](https://en.wikipedia.org/wiki/Random_forest).  The main implementation for R is in the randomForest package, but there are other implementations. See the [CRAN view on Machine Learning](https://CRAN.R-project.org/view=MachineLearning).

## Basic examples - Classification and Regression
        ######  Used for both Classification and Regression examples
        library(randomForest)
        library(car)            ## For the Soils data
        data(Soils)
        
        ######################################################
        ##    RF Classification Example
        set.seed(656)            ## for reproducibility
        S_RF_Class = randomForest(Gp ~ ., data=Soils[,c(4,6:14)])
        Gp_RF = predict(S_RF_Class, Soils[,6:14])
        length(which(Gp_RF != Soils$Gp))            ## No Errors

        ## Naive Bayes for comparison
        library(e1071)
        S_NB  = naiveBayes(Soils[,6:14], Soils[,4]) 
        Gp_NB = predict(S_NB, Soils[,6:14], type="class")
        length(which(Gp_NB != Soils$Gp))            ## 6 Errors
This example tested on the training data, but illustrates that RF can make very good models.

        ######################################################
        ##    RF Regression Example
        set.seed(656)            ## for reproducibility
        S_RF_Reg = randomForest(pH ~ ., data=Soils[,6:14])
        pH_RF = predict(S_RF_Reg, Soils[,6:14])

        ## Compare Predictions with Actual values for RF and Linear Model
        S_LM = lm(pH ~ ., data=Soils[,6:14])
        pH_LM = predict(S_LM, Soils[,6:14])
        par(mfrow=c(1,2))
        plot(Soils$pH, pH_RF, pch=20, ylab="Predicted", main="Random Forest")
        abline(0,1)
        plot(Soils$pH, pH_LM, pch=20, ylab="Predicted", main="Linear Model")
        abline(0,1)
[![Predicted Values vs Actuals for RF and Linear model][1]][1]


  [1]: https://i.stack.imgur.com/ieM8R.png

