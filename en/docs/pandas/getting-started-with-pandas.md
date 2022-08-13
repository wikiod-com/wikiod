---
title: "Getting started with pandas"
slug: "getting-started-with-pandas"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting pandas set up or installed can be found [here in the official documentation][1].

**Installing pandas with Anaconda**

Installing pandas and the rest of the [NumPy][2] and [SciPy][3] stack can be a little difficult for inexperienced users.

The simplest way to install not only pandas, but Python and the most popular packages that make up the SciPy stack (IPython, NumPy, Matplotlib, ...) is with [Anaconda][4], a cross-platform (Linux, Mac OS X, Windows) Python distribution for data analytics and scientific computing.

After running a simple installer, the user will have access to pandas and the rest of the SciPy stack without needing to install anything else, and without needing to wait for any software to be compiled.

Installation instructions for Anaconda [can be found here][5].

A full list of the packages available as part of the Anaconda distribution [can be found here][6].

An additional advantage of installing with Anaconda is that you don’t require admin rights to install it, it will install in the user’s home directory, and this also makes it trivial to delete Anaconda at a later date (just delete that folder).

**Installing pandas with Miniconda**

The previous section outlined how to get pandas installed as part of the Anaconda distribution. However this approach means you will install well over one hundred packages and involves downloading the installer which is a few hundred megabytes in size.

If you want to have more control on which packages, or have a limited internet bandwidth, then installing pandas with [Miniconda][7] may be a better solution.

[Conda][8] is the package manager that the Anaconda distribution is built upon. It is a package manager that is both cross-platform and language agnostic (it can play a similar role to a pip and virtualenv combination).

[Miniconda][7] allows you to create a minimal self contained Python installation, and then use the [Conda][8] command to install additional packages.

First you will need Conda to be installed and downloading and running the Miniconda will do this for you. The installer [can be found here][7].

The next step is to create a new conda environment (these are analogous to a virtualenv but they also allow you to specify precisely which Python version to install also). Run the following commands from a terminal window:

    conda create -n name_of_my_env python

This will create a minimal environment with only Python installed in it. To put your self inside this environment run:

    source activate name_of_my_env

On Windows the command is:

    activate name_of_my_env

The final step required is to install pandas. This can be done with the following command:

    conda install pandas

To install a specific pandas version:

    conda install pandas=0.13.1

To install other packages, IPython for example:

    conda install ipython

To install the full Anaconda distribution:

    conda install anaconda

If you require any packages that are available to pip but not conda, simply install pip, and use pip to install these packages:

    conda install pip
    pip install django


  [1]: http://pandas.pydata.org/pandas-docs/stable/install.html
  [2]: http://www.numpy.org/
  [3]: http://www.scipy.org/
  [4]: http://docs.continuum.io/anaconda/
  [5]: http://docs.continuum.io/anaconda/install.html
  [6]: http://docs.continuum.io/anaconda/pkg-docs.html
  [7]: http://conda.pydata.org/miniconda.html
  [8]: http://conda.pydata.org/docs/

Usually, you would install pandas with one of packet managers.

**pip example:**

    pip install pandas

This will likely require the installation of a number of dependencies, including NumPy, will require a compiler to compile required bits of code, and can take a few minutes to complete.

## Install via anaconda
First [download anaconda](https://www.continuum.io/downloads) from the Continuum site. Either via the graphical installer (Windows/OSX) or running a shell script (OSX/Linux). This includes pandas!

---

If you don't want the 150 packages conveniently bundled in anaconda, you can install [miniconda](http://conda.pydata.org/miniconda.html). Either via the graphical installer (Windows) or shell script (OSX/Linux).

Install pandas on miniconda using:

    conda install pandas

---

To update pandas to the latest version in anaconda or miniconda use:

    conda update pandas

## Descriptive statistics
Descriptive statistics (mean, standard deviation, number of observations, minimum, maximum, and quartiles) of numerical columns can be calculated using the `.describe()` method, which returns a pandas dataframe of descriptive statistics.

    In [1]: df = pd.DataFrame({'A': [1, 2, 1, 4, 3, 5, 2, 3, 4, 1], 
                               'B': [12, 14, 11, 16, 18, 18, 22, 13, 21, 17], 
                               'C': ['a', 'a', 'b', 'a', 'b', 'c', 'b', 'a', 'b', 'a']})
    
    In [2]: df
    Out[2]: 
       A   B  C
    0  1  12  a
    1  2  14  a
    2  1  11  b
    3  4  16  a
    4  3  18  b
    5  5  18  c
    6  2  22  b
    7  3  13  a
    8  4  21  b
    9  1  17  a

    In [3]: df.describe()
    Out[3]:
                   A          B
    count  10.000000  10.000000
    mean    2.600000  16.200000
    std     1.429841   3.705851
    min     1.000000  11.000000
    25%     1.250000  13.250000
    50%     2.500000  16.500000
    75%     3.750000  18.000000
    max     5.000000  22.000000

Note that since `C` is not a numerical column, it is excluded from the output.

    In [4]: df['C'].describe()
    Out[4]:
    count     10
    unique     3
    freq       5
    Name: C, dtype: object

In this case the method summarizes categorical data by number of observations, number of unique elements, mode, and frequency of the mode.

## Hello World
Once Pandas has been installed, you can check if it is is working properly by creating a dataset of randomly distributed values and plotting its histogram.


    import pandas as pd  # This is always assumed but is included here as an introduction.
    import numpy as np
    import matplotlib.pyplot as plt
    
    np.random.seed(0)
    
    values = np.random.randn(100) # array of normally distributed random numbers
    s = pd.Series(values) # generate a pandas series
    s.plot(kind='hist', title='Normally distributed random values') # hist computes distribution
    plt.show()   


 [![enter image description here][1]][1]

Check some of the data's statistics (mean, standard deviation, etc.)

    s.describe()
    # Output: count    100.000000
    # mean       0.059808
    # std        1.012960
    # min       -2.552990
    # 25%       -0.643857
    # 50%        0.094096
    # 75%        0.737077
    # max        2.269755
    # dtype: float64


  [1]: http://i.stack.imgur.com/EbrKm.jpg

