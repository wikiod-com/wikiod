---
title: "Getting started with python-sphinx"
slug: "getting-started-with-python-sphinx"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Since [Sphinx][1] is available on the [Python Package Index][2], it can be installed using [pip][3]:

    pip install Sphinx

or you can also install using easy_install

    easy_install -U sphinx

Then you can check that it has been correctly installed by executing the following command:

    > sphinx-quickstart --version
    Sphinx v1.4.1
    
Before proceeding ahead you need to follow certain code style for python [PEP 8][4]. If you have followed [PEP 8][4] guideline then you can [define document structure][5] in sphinx and start [building your documentation][6].


  [1]: http://www.sphinx-doc.org/en/stable/
  [2]: https://pypi.python.org/pypi/Sphinx
  [3]: https://pypi.python.org/pypi/pip
  [4]: https://www.python.org/dev/peps/pep-0008/
  [5]: http://www.sphinx-doc.org/en/stable/tutorial.html#defining-document-structure
  [6]: http://www.sphinx-doc.org/en/stable/tutorial.html#running-the-build

## Quick Start
To get started go to root of project directory and run :

    $ sphinx-quickstart
You will get options to create documentation for your project.
For default setup follow below commands:

    Prompt    Choice
    > Root path for the documentation [.]:    <ENTER>
    > Separate source and build directories (y/N) [n]:    y
    > Name prefix for templates and static dir [_]:    <ENTER>
    > Project name:    an_example_pypi_project
    > Author name(s):    Andrew Carter
    > Project version:    0.0.1
    > Project release [0.0.1]:    <ENTER>
    > Source file suffix [.rst]:    <ENTER>
    > Name of your master document (without suffix) [index]:    <ENTER>
    > autodoc: automatically insert docstrings from modules (y/N) [n]:    y
    > doctest: automatically test code snippets in doctest blocks (y/N) [n]:    n
    > intersphinx: link between Sphinx documentation of different projects (y/N) [n]:    y
    > todo: write “todo” entries that can be shown or hidden on build (y/N) [n]:    n
    > coverage: checks for documentation coverage (y/N) [n]:    n
    > pngmath: include math, rendered as PNG images (y/N) [n]:    n
    > jsmath: include math, rendered in the browser by JSMath (y/N) [n]:    n
    > ifconfig: conditional inclusion of content based on config values (y/N) [n]:    y
    > Create Makefile? (Y/n) [y]:    n
    > Create Windows command file? (Y/n) [y]:    n
Upon successful execution you may discover config.py file in your doc/source directory of your project. This file has control to basic structure of how your document will generate when you run build command as below

`$ sphinx-build -b html sourcedir builddir`

Detailed instructions are avilable at : https://pythonhosted.org/an_example_pypi_project/sphinx.html

