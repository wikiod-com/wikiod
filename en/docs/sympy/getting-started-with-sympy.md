---
title: "Getting started with sympy"
slug: "getting-started-with-sympy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing SymPy
The easiest and [recommended](http://docs.sympy.org/latest/install.html) way to install SymPy is to install [Anaconda](https://www.continuum.io/downloads). 

If you already have Anaconda or Miniconda installed, you can install the latest version with conda:

    conda install sympy

Another way of installing SymPy is using pip:

    pip install sympy

Note that this might require root privileges, so one might acually need

    sudo pip install sympy

Most linux distributions also offer SymPy in their package repositories. For Fedora one would install SymPy with

    sudo dnf install python-sympy
    sudo dnf install python3-sympy

The first one installs the python 2 version of the package, the latter python 3.

On OpenSuse the respective commands are:

    sudo zypper install python-sympy
    sudo zypper install python3-sympy

The packages for OpenSuse 42.2 seem rather outdated, so one of the first methods should be prefered.

## Integration and Differentiation
Sympy is made for symbolic math, so let's have a look at some basic integration and differentiation.

    from sympy import symbols, sqrt, exp, diff, integrate, pprint                                                                                                                      
    
    x, y = symbols('x y', real=True)                                                                                                                                                              
    
    pprint(diff(4*x**3+exp(3*x**2*y)+y**2,x))                                                                                                                                          
    pprint(diff(4*x**3+exp(3*x**2*y)+y**2,y))                                                                                                                                          
    pprint(integrate(exp(x*y**2)+sqrt(x)*y**2,x))                                                                                                                                      
    pprint(integrate(exp(x*y**2)+sqrt(x)*y**2,y))

First we import the necessary functions from sympy. Next we define our variables x and y. Note that these are considered complex by default, so we tell sympy that we want a simple example by making them real.
Next we differentiate some expression with respect to x and then y.
Finally we integrate some expression, again with respect to x and then y.
The call of `pprint` ensures that our functions get written in some nice human readable style.

## Alternate installation (not conda)
*Alternate ways to install SymPy from conda. conda is the recommended way, but these are some alternate ways. Including: git, pip, etc.* 

## 'Hello World'
**Sympy** is a Python library for doing symbolic — rather than numeric — calculations.

For instance, consider the quadratic equation in **x**,

x**2 + HELLO * x + WORLD = 0

where HELLO and WORLD are constants. What's the solution of this equation?

In Python,  using Sympy we can code,

    from sympy import symbols, solve, latex
    
    x, HELLO, WORLD = symbols('x, HELLO, WORLD')
    print ( latex ( solve ( x**2 + HELLO * x + WORLD, x ) ) )

Since I made a call to Latex the solutions are almost ready for publication! Sympy provides the two of them packed in a list. Here's one:

[![one solution][1]][1]

If you need to do more work on an expression then you would leave out the call to latex.


  [1]: http://i.stack.imgur.com/GTRAH.jpg

