---
title: "Inspecting packages"
slug: "inspecting-packages"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Packages build on base R. This document explains how to inspect installed packages and their functionality. Related Docs: [Installing packages](https://www.wikiod.com/r/installing-packages)

The Comprehensive R Archive Network (CRAN) is the primary [package repository][1].


  [1]: https://cran.r-project.org/web/packages/

## View package information
To retrieve information about dplyr package and its functions' descriptions:

    help(package = "dplyr")
    
No need to load the package first.


## View package's built-in data sets
To see built-in data sets from package dplyr

     data(package = "dplyr")

No need to load the package first.
 

## List a package's exported functions
To get the list of functions within package dplyr, we first must load the package:

    library(dplyr)
    ls("package:dplyr")


## View Package Version
Conditions: package should be at least installed. If not loaded in the current session, not a problem.

      ## Checking package version which was installed at past or 
      ## installed currently but not loaded in the current session

      packageVersion("seqinr")
      # [1] ‘3.3.3’
      packageVersion("RWeka")
      # [1] ‘0.4.29’

      

## View Loaded packages in Current Session
To check the list of loaded packages

    search()

OR

    (.packages())

