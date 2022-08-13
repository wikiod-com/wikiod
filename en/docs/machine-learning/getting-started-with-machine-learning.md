---
title: "Getting started with machine-learning"
slug: "getting-started-with-machine-learning"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup using Python
**1) scikit learn**

scikit-learn is a Python module for machine learning built on top of SciPy and distributed under the 3-Clause BSD license. It features various classification, regression and clustering algorithms including support vector machines, random forests, gradient boosting, k-means and DBSCAN, and is designed to interoperate with the Python numerical and scientific libraries NumPy and SciPy.
 

The current stable version of scikit-learn [requires][1]:
   - Python (>= 2.6 or >= 3.3),
   - NumPy (>= 1.6.1),
   - SciPy (>= 0.9).

For most installation `pip` python package manager can install python and all of its dependencies:
```
pip install scikit-learn
```

However for linux systems it is recommended to use `conda` package manager to avoid possible build processes
```
conda install scikit-learn
```

To check that you have `scikit-learn`, execute in shell:
```
python -c 'import sklearn; print(sklearn.__version__)'
```

**Windows and Mac OSX Installation:**

[Canopy][2] and [Anaconda][3] both ship a recent version of *scikit-learn*, in addition to a large set of scientific python library for Windows, Mac OSX (also relevant for Linux).


Official source code repo: https://github.com/scikit-learn/scikit-learn   

----------
**2) Numenta Platform for Intelligent Computing**

The Numenta Platform for Intelligent Computing (NuPIC) is a machine intelligence platform that implements the HTM learning algorithms. HTM is a detailed computational theory of the neocortex. At the core of HTM are time-based continuous learning algorithms that store and recall spatial and temporal patterns. NuPIC is suited to a variety of problems, particularly anomaly detection and prediction of streaming data sources.

NuPIC binaries are available for:

Linux x86 64bit  
OS X 10.9  
OS X 10.10  
Windows 64bit

The following dependencies are required to install NuPIC on all operating systems.

 - Python 2.7 
 - pip>=8.1.2
 - setuptools>=25.2.0
 - wheel>=0.29.0
 - numpy
 - C++ 11 compiler like gcc (4.8+) or clang

Additional OS X requirements:

 - Xcode command line tools

Run the following to install NuPIC:

    pip install nupic

Official source code repo: https://github.com/numenta/nupic 


----------


**3) nilearn** 

Nilearn is a Python module for fast and easy statistical learning on NeuroImaging data.
It leverages the scikit-learn Python toolbox for multivariate statistics with applications such as predictive modelling, classification, decoding, or connectivity analysis.

The required dependencies to use the software are:

 - Python >= 2.6,
 - setuptools
 - Numpy >= 1.6.1
 - SciPy >= 0.9
 - Scikit-learn >= 0.14.1
 - Nibabel >= 1.1.0

If you are using nilearn plotting functionalities or running the examples, matplotlib >= 1.1.1 is required.  

If you want to run the tests, you need nose >= 1.2.1 and coverage >= 3.6.

First make sure you have installed all the dependencies listed above. Then you can install nilearn by running the following command in a command prompt:

    pip install -U --user nilearn

Official source code repo:  https://github.com/nilearn/nilearn/ 

**4) Using Anaconda**

Many scientific Python libraries are readily available in Anaconda. You can get installation files from [here][3]. On one hand, using Anaconda, you do not to install and configure many packages, it is BSD licensed, and has trivial installation process, available for Python 3 and Python 2, while, on the other hand, it gives you less flexibility. As an example, some state of the art deep learning python packages might use a different version of numpy then Anaconda installed. However, this downside can be dealt with using another python installation separately(In linux and MAC your default one for example). 

 Anaconda setup prompts you to installation location selection and also prompts you to PATH addition option. If you add Anaconda to your PATH it is expected that your OS will find Anaconda Python as default. Therefore, modifications and future installations will be available for this Python version only. 

 To make it clear, after installation of Anaconda and you add it to PATH, using Ubuntu 14.04 via terminal if you type 
 
    python

[![Anaconda Python via Terminal][4]][4]

Voila, Anaconda Python is your default Python, you can start enjoying using many libraries right away. However, if you want to use your old Python

 

    /usr/bin/python

[![Default Python via Terminal][5]][5]
 

 In long story short, Anaconda is one of the fastest way to start machine learning and data analysis with Python.  



 


  [1]: http://scikit-learn.org/stable/install.html
  [2]: https://www.enthought.com/products/canopy/
  [3]: https://www.continuum.io/downloads
  [4]: https://i.stack.imgur.com/U9ot4.jpg
  [5]: https://i.stack.imgur.com/hhJUO.jpg

## Installation or Setup using R Language
[Packages](https://www.wikiod.com/r/installing-packages#Download and install packages from repositories) are collections of R functions, data, and compiled code in a well-defined format. Public (and private) repositories are used to host collections of R packages. The largest collection of R packages is available from CRAN. Some of the most popular R machine learning packages are the following among others:

**1) rpart**

Description: Recursive partitioning for classification, regression and survival trees. An implementation of most of the functionality of the 1984 book by Breiman, Friedman, Olshen and Stone.

It can be installed from CRAN using the following code:

    install.packages("rpart")
Load the package:    

    library(rpart)

Official source: https://cran.r-project.org/web/packages/rpart/index.html


----------


**2) e1071**

Description: Functions for latent class analysis, short time Fourier transform, fuzzy clustering, support vector machines, shortest path computation, bagged clustering, naive Bayes classifier etc.

Installation from CRAN:

    install.packages("e1071")

Loading the package:    

    library(e1071)

Official source: https://cran.r-project.org/web/packages/e1071/index.html


----------

**3) **randomForest****

Description: Classification and regression based on a forest of trees using random inputs.

Installation from CRAN:

    install.packages("randomForest")

Loading the package:    

    library(randomForest)

Official source: https://cran.r-project.org/web/packages/randomForest/index.html


----------
**4) caret**

Description: Misc functions for training and plotting classification and regression models.

Installation from CRAN:

    install.packages("caret")

Loading the package:    

    library(caret)

Official source: https://cran.r-project.org/web/packages/caret/index.html

