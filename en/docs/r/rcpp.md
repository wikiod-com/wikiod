---
title: "Rcpp"
slug: "rcpp"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Extending Rcpp with Plugins
Within C++, one can set different compilation flags using:

<!-- language: cpp -->
     // [[Rcpp::plugins(name)]]

List of the built-in plugins:

<!-- language: cpp -->

    // built-in C++11 plugin
    // [[Rcpp::plugins(cpp11)]]

    // built-in C++11 plugin for older g++ compiler
    // [[Rcpp::plugins(cpp0x)]]

    // built-in C++14 plugin for C++14 standard
    // [[Rcpp::plugins(cpp14)]]

    // built-in C++1y plugin for C++14 and C++17 standard under development
    // [[Rcpp::plugins(cpp1y)]]

    // built-in OpenMP++11 plugin
    // [[Rcpp::plugins(openmp)]]

## Inline Code Compile
Rcpp features two functions that enable code compilation inline and exportation directly into R: `cppFunction()` and `evalCpp()`. A third function called `sourceCpp()` exists to read in C++ code in a separate file though can be used akin to `cppFunction()`.

Below is an example of compiling a C++ function within *R*. Note the use of `""` to surround the source.

    # Note - This is R code.
    # cppFunction in Rcpp allows for rapid testing.
    require(Rcpp)

    # Creates a function that multiples each element in a vector
    # Returns the modified vector.
    cppFunction("
    NumericVector exfun(NumericVector x, int i){
    x = x*i;
    return x;
    }")

    # Calling function in R
    exfun(1:5, 3)


To quickly understand a C++ expression use:

    # Use evalCpp to evaluate C++ expressions
    evalCpp("std::numeric_limits<double>::max()")
    ## [1] 1.797693e+308

## Rcpp Attributes
Rcpp Attributes makes the process of working with R and C++ straightforward. The form of attributes take:

<!-- language: cpp -->

    // [[Rcpp::attribute]]

The use of attributes is typically associated with:

<!-- language: cpp -->

    // [[Rcpp::export]]

that is placed directly above a declared function header when reading in a C++ file via `sourceCpp()`. 

Below is an example of an external C++ file that uses attributes.

<!-- language: cpp -->

    // Add code below into C++ file Rcpp_example.cpp
    
    #include <Rcpp.h>
    using namespace Rcpp;
    
    // Place the export tag right above function declaration.
    // [[Rcpp::export]]
    double muRcpp(NumericVector x){

        int n = x.size(); // Size of vector
        double sum = 0; // Sum value

        // For loop, note cpp index shift to 0
        for(int i = 0; i < n; i++){
            // Shorthand for sum = sum + x[i]
            sum += x[i];
        }

        return sum/n; // Obtain and return the Mean
    }

    // Place dependent functions above call or
    // declare the function definition with:
    double muRcpp(NumericVector x);

    // [[Rcpp::export]]
    double varRcpp(NumericVector x, bool bias = true){

        // Calculate the mean using C++ function
        double mean = muRcpp(x);
        double sum = 0;

        int n = x.size();

        for(int i = 0; i < n; i++){
            sum += pow(x[i] - mean, 2.0); // Square
        }

        return sum/(n-bias); // Return variance
    }

To use this external *C++* file within *R*, we do the following:


    require(Rcpp)

    # Compile File
    sourceCpp("path/to/file/Rcpp_example.cpp")

    # Make some sample data
    x = 1:5

    all.equal(muRcpp(x), mean(x))
    ## TRUE

    all.equal(varRcpp(x), var(x))
    ## TRUE

## Specifying Additional Build Dependencies
To use additional packages within the Rcpp ecosystem, the correct header file may not be `Rcpp.h` but `Rcpp<PACKAGE>.h` (as _e.g._ for [RcppArmadillo](https://cloud.r-project.org/web/packages/RcppArmadillo/index.html)). It typically needs to be imported and then the dependency is stated within 

<!-- language: cpp -->

    // [[Rcpp::depends(Rcpp<PACKAGE>)]]

Examples:

<!-- language: cpp -->

    // Use the RcppArmadillo package
    // Requires different header file from Rcpp.h
    #include <RcppArmadillo.h>
    // [[Rcpp::depends(RcppArmadillo)]]

    // Use the RcppEigen package
    // Requires different header file from Rcpp.h
    #include <RcppEigen.h>
    // [[Rcpp::depends(RcppEigen)]]

