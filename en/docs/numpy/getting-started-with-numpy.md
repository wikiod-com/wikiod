---
title: "Getting started with numpy"
slug: "getting-started-with-numpy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Import
Import the numpy module to use any part of it.

    import numpy as np

Most examples will use `np` as shorthand for numpy. Assume "np" means "numpy" in code examples.

    x = np.array([1,2,3,4])



## Installation on Linux
NumPy is available in the default repositories of most popular Linux distributions and can be installed in the same way that packages in a Linux distribution are usually installed.

Some Linux distributions have different NumPy packages for Python 2.x and Python 3.x. In Ubuntu and Debian, install `numpy` at the system level using the APT package manager:

    sudo apt-get install python-numpy  
    sudo apt-get install python3-numpy  

For other distributions, use their package managers, like zypper (Suse), yum (Fedora) etc.

`numpy` can also be installed with Python's package manager `pip` for Python 2 and with `pip3` for Python 3:

    pip install numpy  # install numpy for Python 2
    pip3 install numpy  # install numpy for Python 3


`pip` is available in the default repositories of most popular Linux distributions and can be installed for Python 2 and Python 3 using:

    sudo apt-get install python-pip  # pip for Python 2
    sudo apt-get install python3-pip  # pip for Python 3

After installation, use `pip` for Python 2 and `pip3` for Python 3 to use pip for installing Python packages.
But note that you might need to install many dependencies, which are required to build numpy from source (including development-packages, compilers, fortran etc).

Besides installing `numpy` at the system level, it is also common (perhaps even highly recommended) to install `numpy` in virtual environments using popular Python packages such as `virtualenv`. In Ubuntu, `virtualenv` can be installed using:

    sudo apt-get install virtualenv

Then, create and activate a virtualenv for either Python 2 or Python 3 and then use `pip` to install `numpy`:

    virtualenv venv  # create virtualenv named venv for Python 2
    virtualenv venv -p python3  # create virtualenv named venv for Python 3
    source venv/bin/activate  # activate virtualenv named venv
    pip install numpy  # use pip for Python 2 and Python 3; do not use pip3 for Python3

## Installation on Windows
Numpy installation through [pypi][1] (the default package index used by pip) generally fails on Windows computers.  The easiest way to install on Windows is by using precompiled binaries.

One source for precompiled wheels of many packages is [Christopher Gohkle's site][2]. Choose a version according to your Python version and system.
An example for Python 3.5 on a 64 bit system:

  1. Download `numpy-1.11.1+mkl-cp35-cp35m-win_amd64.whl` from [here][2]
  2. Open a Windows terminal (cmd or powershell) 
  3. Type the command `pip install C:\path_to_download\numpy-1.11.1+mkl-cp35-cp35m-win_amd64.whl`

If you don't want to mess around with single packages, you can use the [Winpython distribution](https://winpython.github.io/) which bundles most packages together and provides a confined environment to work with.  Similarly, the [Anaconda Python distrubution][6] comes pre-installed with numpy and numerous other common packages.  

Another popular source is the [`conda` package manager][3], which also supports [virtual environments][4]. 

  1. Download and install [`conda`][5].
  2. Open a Windows terminal.
  3. Type the command `conda install numpy`



  [1]: https://pypi.python.org/pypi
  [2]: http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy
  [3]: http://conda.pydata.org/docs/intro.html
  [4]: http://docs.python-guide.org/en/latest/dev/virtualenvs/
  [5]: http://conda.pydata.org/miniconda.html
  [6]: https://www.continuum.io/downloads

## Installation on Mac
The easiest way to set up NumPy on Mac is with [pip](https://pip.pypa.io/en/stable/installing/) 

    pip install numpy  

**Installation using Conda**.  
Conda available for Windows, Mac, and Linux

 1. Install Conda. There are two ways to install Conda, either with Anaconda (Full package, include numpy) or Miniconda (only Conda,Python, and the packages they depend on, without any additional package). Both Anaconda & Miniconda install the same Conda.
 2. Additional command for Miniconda, type the command `conda install numpy`

## Temporary Jupyter Notebook hosted by Rackspace
[Jupyter Notebooks](http://jupyter.org/) are an interactive, browser-based development environment. They were originally developed to run computation python and as such play very well with numpy. To try numpy in a Jupyter notebook without fully installing either on one's local system Rackspace provides free temporary notebooks at [tmpnb.org](http://tmpnb.org). 

**Note:** that this is not a proprietary service with any sort of upsells. Jupyter is a wholly open-sourced technology developed by UC Berkeley and Cal Poly San Luis Obispo. Rackspace donates this [service](https://github.com/jupyter/tmpnb) as part of the development process.

To try `numpy` at tmpnb.org:

1. visit [tmpnb.org](http://tmpnb.org)
2. either select `Welcome to Python.ipynb` or
3. New >> Python 2 or
4. New >> Python 3

