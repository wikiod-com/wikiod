---
title: "Getting started with scipy"
slug: "getting-started-with-scipy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Convert a sparse matrix to a dense matrix using SciPy
     from scipy.sparse import csr_matrix
     A = csr_matrix([[1,0,2],[0,3,0]])
     >>>A
     <2x3 sparse matrix of type '<type 'numpy.int64'>'
        with 3 stored elements in Compressed Sparse Row format>
     >>> A.todense()
       matrix([[1, 0, 2],
               [0, 3, 0]])
     >>> A.toarray()
          array([[1, 0, 2],
                [0, 3, 0]])



## Image Manipulation using Scipy (Basic Image resize)
SciPy provides basic image manipulation functions. These include functions to read images from disk into numpy arrays, to write numpy arrays to disk as images, and to resize images.

In the following code, only one image is used. It is tinted, resized, and saved. Both original and resulting images are shown below:  

    import numpy as np  //scipy is numpy-dependent

    from scipy.misc import imread, imsave, imresize   //image resizing functions
    
    # Read an JPEG image into a numpy array
    img = imread('assets/cat.jpg')
    print img.dtype, img.shape  # Prints "uint8 (400, 248, 3)"
    
    # We can tint the image by scaling each of the color channels
    # by a different scalar constant. The image has shape (400, 248, 3);
    # we multiply it by the array [1, 0.95, 0.9] of shape (3,);
    # numpy broadcasting means that this leaves the red channel unchanged,
    # and multiplies the green and blue channels by 0.95 and 0.9
    # respectively.
    img_tinted = img * [1, 0.95, 0.9]
    
    # Resize the tinted image to be 300 by 300 pixels.
    img_tinted = imresize(img_tinted, (300, 300))
    
    # Write the tinted image back to disk
    imsave('assets/cat_tinted.jpg', img_tinted)


[![original][1]][1]  [![resized_tinted][2]][2]

[Reference][3]


  [1]: http://i.stack.imgur.com/K16nx.jpg
  [2]: http://i.stack.imgur.com/RP5UX.jpg
  [3]: http://cs231n.github.io/python-numpy-tutorial/#scipy

## Basic Hello World
Create a file (e.g. hello_world.py) in a text editor or a python editor if you have one installed ([pick one if you don't][1] - SublimeText, Eclipse, NetBeans, SciTe... there's many!)

    hwld = 'Hello world'
    print(hwld)

Note that python variables do not need to be explicitly declared; the declaration happens when you assign a value with the equal (=) sign to a variable. 

The output of the above two lines of code is that the string "Hello World" will be displayed.

Functions written in Python can be used in iPython also.

In this instance, you can use run your saved file 'hello_world.py' in IPython like so:

    In [1]: %run hello_world.py  
    #run file to get output below
    Hello world
    In [2]: wld   
    #show what value of wld var is
    Out[2]: 'Hello world'
    In [3]: %whowld  
    #display info on variable wld (name/type/value)
    
    Variable     Type     Data/Info
    ----------------------------
    wld         str     Hello world

If you wish you can use two variables, e.g one for hello and one for world and concatenate them using the plus (+) sign:

     h = 'Hello '
     w = "world!'
     print(h+w)

     #this will also output Hello World, only this time with an exclamation mark..


   

  [1]: https://wiki.python.org/moin/PythonEditors

## Versions
The first release of SciPy, vsn 0.10, was released on August 14th 2001. The current release of SciPy (correct at 26th July 2016) is v 0.17 (stable) with v .18 forthcoming soon. Details of former releases are listed [here][1]


  [1]: https://github.com/scipy/scipy/releases

## Installation or Setup
Scipy contains parts written in C, C++, and Fortran that need to be compiled before use. Therefore make sure the necessary compilers and Python development headers are installed. Having compiled code also means that Scipy needs additional steps to import from development sources, which are explained below.

Fork a copy of the main Scipy repository in Github onto your own account, then create your local repository via:

    $ git clone git@github.com:YOURUSERNAME/scipy.git scipy
    $ cd scipy
    $ git remote add upstream git://github.com/scipy/scipy.git

To build the development version of Scipy and run tests, spawn interactive shells with the Python import paths properly set up, and so on. Do one of the following:

    $ python runtests.py -v
    $ python runtests.py -v -s optimize
    $ python runtests.py -v -t scipy/special/tests/test_basic.py:test_xlogy
    $ python runtests.py --ipython
    $ python runtests.py --python somescript.py
    $ python runtests.py --bench

This builds Scipy first, so it may take a while the first time. Specifying `-n` will run the tests against the version of Scipy (if any) found on the current PYTHONPATH.

Using runtests.py is the recommended approach to running tests. There are also a number of alternatives to it, for example in-place build or installing to a virtual environment. Some tests are very slow and need to be separately enabled.

[Link to API][1]

**Ubuntu & Debian**

Run command

    sudo apt-get install python-numpy python-scipy python-matplotlib ipython ipython-notebook python-pandas python-sympy python-nose

The versions in Ubuntu 12.10 or newer and Debian 7.0 or newer meet the current SciPy stack specification. Users might also want to add the [NeuroDebian][2] repository for extra SciPy packages.


  [1]: https://docs.scipy.org/doc/scipy/reference/api.html
  [2]: http://neuro.debian.net/

