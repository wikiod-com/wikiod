---
title: "Plot a subset of data"
slug: "plot-a-subset-of-data"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- xlim(left.limit,right.limit)
- data.frame[data.frame$variable == "desired.variable",]

## Using xlim / ylim
    > library(ggplot2)
    > ggplot(iris,aes(Sepal.Width)) + geom_density() + xlim(1,3.5)

Using xlim or ylim the plot is not cutted, ggplot subsets the data before 
calling the stat function (stat_density in this case). You can see it in the warning message. 


    Warning message:
    Removed 19 rows containing non-finite values (stat_density).

## Inline Subsetting for categorical variables
    ggplot(iris[iris$Species == "setosa",],aes(Sepal.Width)) + 
        geom_density()

Here, we are subsetting the dataframe before passing it to ggplot. It is a very useful tool derived from the data frame data structure. 

To make the code more readable, one can also use `dplyr`'s `filter`:

    library(dplyr)
    iris %>% filter(Species == "setosa") %>% ggplot(aes(Sepal.Width)) + 
        geom_density()

