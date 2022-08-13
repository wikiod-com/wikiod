---
title: "Getting started with ipython"
slug: "getting-started-with-ipython"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Usage
Like the built-in `python` interactive shell, [IPython][2] is a REPL ([Read-Evaluate-Print Loop][3]) shell, with a variety of features that make it more pleasant to use for day-to-day Python development than the built-in REPL shell.

# Installation

To install it:

    pip install ipython

Or, via [Anaconda][4]:

    # To install into the active environment:
    $ conda install ipython 

    # Or, to create a new environment with IPython installed:
    $ conda create -n <env_name> ipython

Or, via [Enthought Canopy][5]:

    $ enpkg ipython

# Usage

After installation, run it using your default Python (2 or 3) using:

    ipython

Or to use Python 3:

    ipython3


[![ipython shell][1]][1]

  [1]: http://i.stack.imgur.com/rlKDr.png
  [2]: https://ipython.org/
  [3]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
  [4]: https://docs.continuum.io/
  [5]: https://www.enthought.com/products/canopy/

## Getting Help
    ?

This gives you an introduction and overview of IPython's features.

    object? 

This lists all methods and fields of the object and its documentation (if it exists).

    object??

Same as above, provides even more detail about the object, in particular will try to find and display the source code for the object. 

    object.<TAB Key>

<kbd>TAB</kbd>-completion that lists and iterates through available fields/methods of an object. Due to the dynamic nature of Python not all methods can be discovered this way. Also private methods (starting with `_`) will be hidden by default, insert a `_` and press <kbd>TAB</kbd> again to display them. 

    %quickref

This displays a quick-reference for the IPython shell. 


## IPython vs Jupyter
IPython has two parts to it: A command line interface that replaces the default `python` REPL and a way to run Python through the web browser as a graphical user interface.

With the latest developments the browser part has been split into the [Jupyter](http://jupyter.org/) project that enables multiple programming languages to use the graphical interface. It is still possible to use IPython as a Python kernel for this.

Up to date setup instructions for Jupyter can be found in [the official install docs](http://jupyter.readthedocs.io/en/latest/install.html).

`ipython`, or `jupyter console`, when invoked from the command line without any *other* parameters will enter an interactive terminal session as below:
[![Jupyter Console][1]][1]

`jupyter qtconsole`, *or `ipython qtconsole` before version 5*, will start a multi-tabbed QT based console:   

[![QT Console][2]][2]

`jupyter notebook`, *or `ipython notebook` before version 5*, will start a server and by default open up a web page, at `http://localhost:8888/tree`, with the "Home" view of the current directory. This allows you to open existing notebooks or new kernels *in several languages, depending on which you have installed*; each will be opened in a new browser tab.

Notebooks allow you to mix markdown, including [MathJax][3], code from the kernel of your choice, plots and graphs, images and even videos.
[![Home View][4]][4][![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/nJdwJ.png
  [2]: http://i.stack.imgur.com/wnEst.png
  [3]: https://www.mathjax.org/
  [4]: http://i.stack.imgur.com/U9gMx.png
  [5]: http://i.stack.imgur.com/aBUuo.png

## Pasting into IPython
    %paste

[![%paste demo][1]][1]

This is the primary Magic Method for pasting. It directly pastes text from the system clipboard, intelligently handling common issues with newlines and indentation. 

    %cpaste

[![%cpaste demo][2]][2]

If you are using IPython via SSH, use `%cpaste` instead, as it does not need to access the remote system clipboard.

Since IPython 5.0.0, the improved prompt toolkit should directly handle pasting multi-line code without the need for `%paste` or `%cpaste`.


  [1]: http://i.stack.imgur.com/XRvdz.png
  [2]: http://i.stack.imgur.com/6SCzb.png

## Store variables on IPython
`%storemagic` stores variables and macros on IPython's database. 
To automatically restore stored variables at startup add this to `ipython_config.py`:

    c.StoreMagic.autorestore = True

Example:

    In [1]: l = ['hello',10,'world']
    In [2]: %store l
    In [3]: exit
    
    (IPython session is closed and started again...)
    
    ville@badger:~$ ipython
    In [1]: l
    Out[1]: ['hello', 10, 'world']

Note:

It should be noted that if you change the value of a variable, you need to %store it again if you want to persist the new value.

Note also that the variables will need to be pickleable; most basic python types can be safely %store’d.




