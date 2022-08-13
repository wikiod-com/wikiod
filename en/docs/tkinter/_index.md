---
title : tkinter Tutorial
slug : tkinter-tutorial
weight : 9863
draft : false
images : []
type : docs
---

Tkinter ("**Tk Inter**face")is python's standard cross-platform package for creating graphical user interfaces (GUIs). It provides access to an underlying Tcl interpreter with the Tk toolkit, which itself is a cross-platform, multilanguage graphical user interface library. 

Tkinter isn't the only GUI library for python, but it is the one that comes standard. Additional GUI libraries that can be used with python include [wxPython][1], [PyQt][2], and [kivy][3]. 

Tkinter's greatest strength is its ubiquity and simplicity. It works out of the box on most platforms (linux, OSX, Windows), and comes complete with a wide range of widgets necessary for most common tasks (buttons, labels, drawing canvas, multiline text, etc). 

As a learning tool, tkinter has some features that are unique among GUI toolkits, such as named fonts, bind tags, and variable tracing. 

# Differences between python 2 and 3

Tkinter is largely unchanged between python 2 and python 3, with the major difference being that the tkinter package and modules were renamed. 

## Importing in python 2.x

In python 2.x, the tkinter package is named `Tkinter`, and related packages have their own names. For example, the following shows a typical set of import statements for python 2.x:

    import Tkinter as tk
    import tkFileDialog as filedialog
    import ttk

## Importing in python 3.x

Although functionality did not change much between python 2 and 3, the names of all of the tkinter modules have changed. The following is a typical set of import statements for python 3.x:

    import tkinter as tk
    from tkinter import filedialog
    from tkinter import ttk


# Further Reading

* [Tkinter questions on Stackoverflow][4]
* [Official Python 3 tkinter documentation][5]
* [Official Python 2 tkinter documentation][6]
* [Tkdocs.com - multiplatform tk documentation][7]
* [Effbot introduction to tkinter][8]
* [Tkinter reference guide, New Mexico Tech][9]


  [1]: https://wxpython.org/what.php
  [2]: https://riverbankcomputing.com/software/pyqt/intro
  [3]: https://kivy.org/#home
  [4]: http://stackoverflow.com/questions/tagged/tkinter
  [5]: https://docs.python.org/3.5/library/tkinter.html
  [6]: https://docs.python.org/2.7/library/tkinter.html
  [7]: http://tkdocs.com
  [8]: http://effbot.org/tkinterbook/tkinter-index.htm#introduction
  [9]: http://infohost.nmt.edu/tcc/help/pubs/tkinter/web/index.html

