---
title: "Writing functions in R"
slug: "writing-functions-in-r"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Anonymous functions


## RStudio code snippets
This is just a small hack for those who use self-defined functions often.  
Type "fun" RStudio IDE and hit TAB.  

[![enter image description here][1]][1]  

The result will be a skeleton of a new function.

    name <- function(variables) {
            
    }
***
One can easily define their own snippet template, i.e. like the one below

    name <- function(df, x, y) {
            require(tidyverse)
            out <- 
            return(out)
    }

The option is `Edit Snippets` in the `Global Options -> Code` menu.



  [1]: https://i.stack.imgur.com/gA8QV.png

## Named functions


## Passing column names as argument of a function
Sometimes one would like to pass names of columns from a data frame to a function. They may be provided as strings and used in a function using `[[`. Let's take a look at the following example, which prints to R console basic stats of selected variables:

    basic.stats <- function(dset, vars){
        for(i in 1:length(vars)){
            print(vars[i])
            print(summary(dset[[vars[i]]]))
        }
    }

    basic.stats(iris, c("Sepal.Length", "Petal.Width"))

As a result of running above given code, names of selected variables and their basic summary statistics (minima, first quantiles, medians, means, third quantiles and maxima) are printed in R console. The code `dset[[vars[i]]]` selects i-th element from the argument `vars` and selects a corresponding column in declared input data set `dset`. For example, declaring `iris[["Sepal.Length"]]` alone would print the `Sepal.Length` column from the `iris` data set as a vector.

