---
title: "How to use CPython Scripting in Weka?"
slug: "how-to-use-cpython-scripting-in-weka"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

# How to install CPython in Weka? 

**Install wekaPython**
1. go to `tools`, open `package manager`
2. search `wekaPython`, select and click to install

**Install Python libraries**  
1. install anaconda or conda
2. install four packages: numpy, pandas, matplotlib, scikit-learn  
3. for full installation doc see [conda](http://conda.pydata.org/docs/install/quick.html#os-x-miniconda-install)   



## Hello World Example for CPython of Weka
> Go to `Explorer`, Open `iris.arff` data, then go to `CPython Scripting`, Copy and Paste the following lines of codes into `Python Scripts`: 

    hi = "Hello, CPython of Weka!"
    hello = hi.upper()
    iris = py_data
    info = iris.describe()

> To see output, go to `Python Variables`, select `hi`, for example, and click `Get text`

