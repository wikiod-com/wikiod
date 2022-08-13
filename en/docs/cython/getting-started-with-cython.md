---
title: "Getting started with cython"
slug: "getting-started-with-cython"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
A Cython pyx file needs to be translated to C code (*cythonized*) and compiled before it can be used from Python. A common approach is to create an extension module which is then imported in a Python program.

# Code # 
For this example we create three files:
* `hello.pyx` contains the Cython code.
* `test.py` is a Python script that uses the hello extension.
* `setup.py` is used to compile the Cython code.

## hello.pyx ##

    from libc.math cimport pow

    cdef double square_and_add (double x):
        """Compute x^2 + x as double.

        This is a cdef function that can be called from within
        a Cython program, but not from Python.
        """
        return pow(x, 2.0) + x

    cpdef print_result (double x):
        """This is a cpdef function that can be called from Python."""
        print("({} ^ 2) + {} = {}".format(x, x, square_and_add(x)))

## test.py ## 

    # Import the extension module hello.
    import hello

    # Call the print_result method 
    hello.print_result(23.0)

## setup.py ##
    from distutils.core import Extension, setup
    from Cython.Build import cythonize

    # define an extension that will be cythonized and compiled
    ext = Extension(name="hello", sources=["hello.pyx"])
    setup(ext_modules=cythonize(ext))


# Compiling #

This can be done by, using `cython hello.pyx` to translate the code to C and then compile it using `gcc`. An easier way is to let distutils handle this:

    $ ls
    hello.pyx  setup.py  test.py
    $ python setup.py build_ext --inplace
    $ ls
    build  hello.c  hello.cpython-34m.so  hello.pyx  setup.py  test.py

The shared object (.so) file can be imported and used from Python, so now we can run the `test.py`:

    $ python test.py
    (23.0 ^ 2) + 23.0 = 552.0 

## Installing Cython
To use Cython two things are needed.The Cython package itself, which contains the `cython` source-to-source compiler and Cython interfaces to several C and Python libraries (for example numpy). To compile the C code generated by the `cython` compiler, a C compiler is needed.

# Step 1: Installing Cython #

## System Agnostic ##

[Cython](http://cython.org/#download) can be installed with several system agnostic package management systems. These include:

1. [PyPI](http://pypi.python.org/pypi/Cython/) via pip or easy_install:

       $ pip install cython
       $ easy_install cython

2. [anaconda](https://anaconda.org/anaconda/cython) using conda:

       $ conda install cython

3. Enthought canopy using the enpkg package manager:

       $ enpkg cython

Also the source code can be downloaded from [github](https://github.com/cython/cython) and installed manually using:

    $ python setup.py install

## Ubuntu, Debian ##

For Ubuntu the packages ``cython`` and ``cython3`` are available. Note that these provide an older version than the installation options mentioned above. 

    $ apt-get install cython cython3

## Windows ##

For Windows, a [.whl file](http://www.lfd.uci.edu/~gohlke/pythonlibs/#cython) that can be installed using pip is provided by a third party. Details on installing a .whl file on Windows can be found [here][2].

----

# Step 2: Installing a C Compiler #
To compile the C files generated by Cython, a compiler for C and C++ is needed. The gcc compiler is recommended and can be installed as follows.

## Ubuntu, Debian ##
The ``build-essential`` package contains everything that is needed. It can be installed from the repositories using:

    $ sudo apt-get install build-essential

## MAC ##
The [XCode developer tools](https://developer.apple.com/xcode/) contain a gcc like compiler. 

### Windows ###
[MinGW](http://www.mingw.org/) (Minimalist GNU for Windows) contains a Windows version of gcc. The compiler from Visual Studio can also be used.


  [1]: http://cython.org/#download
  [2]: http://stackoverflow.com/questions/27885397/how-do-i-install-a-python-package-with-a-whl-file
