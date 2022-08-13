---
title : Python Language Tutorial
slug : python-language-tutorial
weight : 566
draft : false
images : []
type : docs
---

[![Python logo](http://i.stack.imgur.com/Bews8.png)  
Python][Python] is a widely used programming language. It is:

 - **High-level**: Python automates low-level operations such as memory management. It leaves the programmer with a bit less control but has many benefits including code readability and minimal code expressions.

 - **General-purpose**: Python is built to be used in all contexts and environments. An example for a non-general-purpose language is PHP: it is designed specifically as a server-side web-development scripting language. In contrast, Python *can* be used for server-side web-development, but also for building desktop applications.

 - **Dynamically typed**: Every variable in Python can reference any type of data. A single expression may evaluate to data of different types at different times. Due to that, the following code is possible:

       if something:
           x = 1
       else:
           x = 'this is a string'
       print(x)

 - **Strongly typed**: During program execution, you are not allowed to do anything that's incompatible with the type of data you're working with. For example, there are no hidden conversions from strings to numbers; a string made out of digits will never be treated as a number unless you convert it explicitly:

       1 + '1'  # raises an error
       1 + int('1')  # results with 2

 - **Beginner friendly :)**: Python's syntax and structure are very intuitive. It is high level and provides constructs intended to enable writing clear programs on both a small and large scale. Python supports multiple programming paradigms, including object-oriented, imperative and functional programming or procedural styles. It has a large, comprehensive standard library and many easy-to-install 3rd party libraries.

Its design principles are outlined in [*The Zen of Python*][PEP20].

Currently, there are two major release branches of Python which have some significant differences. Python 2.x is the legacy version though it still sees widespread use. Python 3.x makes a set of backwards-incompatible changes which aim to reduce feature duplication. For help deciding which version is best for you, see [this article][2v3].

The [official Python documentation][1] is also a comprehensive and useful resource, containing documentation for all versions of Python as well as tutorials to help get you started.

There is one official implementation of the language supplied by Python.org, generally referred to as CPython, and several alternative implementations of the language on other runtime platforms. These include [IronPython][IronPython] (running Python on the .NET platform), [Jython][Jython] (on the Java runtime) and [PyPy][PyPy] (implementing Python in a subset of itself).

  [Python]: https://www.python.org/
  [PEP20]: https://www.python.org/dev/peps/pep-0020/
  [2v3]: https://wiki.python.org/moin/Python2orPython3
  [IronPython]: http://ironpython.net/
  [Jython]: http://www.jython.org/
  [PyPy]: http://pypy.org/

  [1]: https://docs.python.org

