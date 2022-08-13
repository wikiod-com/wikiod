---
title: "Getting started with wxpython"
slug: "getting-started-with-wxpython"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of wxPython Phoenix
[wxPython Phoenix][1] is the latest version of wxPython, (currently *Sept 2016* without an official release). It supports both Python 2 and Python 3. You can download a snapshot build (i.e. a Python wheel) for your platform and Python version [here][2].

wxPython  Phoenix utilizes a largely automated mechanism for generating both the python bindings for the wxWidgets library and the documentation. [Phoenix wxPython documentation][3] is specifically generated for itself using [Sphinx][4]. This increases clarity as opposed to C++ documentation of the classic build, which includes many overloads that are not available in wxPython.

**Python** and **pip** must be installed before wxPython Phoenix can be installed.

You can use pip to install the Phoenix version of wxPython. Here is the recommended method currently:

    python -m pip install --no-index --find-links=http://wxpython.org/Phoenix/snapshot-builds/ --trusted-host wxpython.org wxPython_Phoenix

When you use this command, pip will also install **wxWidgets**. This complex pip command will likely become 'pip install wxpython' when Phoenix is officially released.

*Note: wxPython Phoenix is currently in beta and doesn't have all the widgets that the Classic version has.* 


  [1]: https://wiki.wxpython.org/ProjectPhoenix
  [2]: http://wxpython.org/Phoenix/snapshot-builds/
  [3]: https://wxpython.org/Phoenix/docs/html/main.html
  [4]: http://sphinx-doc.org/

## Installation of wxPython Classic
wxPython Classic is a **Python 2** build of the wxPython library. Generation of the python bindings require a large number of manual interventions and the documentation is simply the wxWidgets documentation which contains some annotations on wxPython mechanisms as such there is normally a delay of weeks to months between a new release of wxWidgets and the matching release of wxPython.

Go to the [download][1] page on the wxPython website to see if there is already a version of wxPython that you can download for your platform.

The latest version of Classic is **3.0.2.0**

**Windows**

There are installers for Python 2.6 and 2.7 for 32-bit and 64-bit Windows platforms on the website. Just download one of these and run them to install it. 

*Note: Make sure you download a wxPython installer for the right Python you have installed. For example, if you have Python 2.7 32-bit, then you want a wxPython 32-bit installer*

**Mac**

If you have OSX **10.5 or above**, then you will want to download and install the **Cocoa** version of wxPython. The Cocoa version also supports 64-bit Mac. 

If you have a Mac with a version of OSX less than **10.5**, then you will want the **Carbon** build.

**Linux**

The first thing to check if your Linux platform's package manager (i.e. yum, apt-get, etc) to see if it has a version of wxPython that you can install. Unfortunately, a lot of Linux packages for wxPython are for version 2.8.12.1 instead of 3.0.2.0. If your package manager doesn't have the latest version, you will probably have to build it yourself.

There are build instructions for 3.0.2.0-Classic [here][2]


  [1]: https://wxpython.org/download.php
  [2]: https://wxpython.org/builddoc.php

## Hello World
A simple way to create a **Hello World** program:

    import wx
    app = wx.App(redirect=False)
    frame = wx.Frame(parent=None, id=wx.ID_ANY, title='Hello World')
    frame.Show()
    app.MainLoop()

Output:

[![Hello World output][1]][1]

A more typical example would be to subclass **wx.Frame**:

    import wx
    
    class MyFrame(wx.Frame):
    
        def __init__(self):
            wx.Frame.__init__(self, None, title='Hello World')
            self.Show()
    
    if __name__ == '__main__':
        app = wx.App(redirect=False)
        frame = MyFrame()
        app.MainLoop()

This can also be rewritten to use Python's **super**:

    import wx  
    
    class MyFrame(wx.Frame):
            
        def __init__(self, *args, **kwargs):
            """Constructor"""
            super(MyFrame, self).__init__(*args, **kwargs)
            self.Show()
    
    if __name__ == '__main__':
        app = wx.App(False)
        frame = MyFrame(None, title='Hello World')
        app.MainLoop()

  [1]: http://i.stack.imgur.com/BxlIr.png

## What is a wxPython Release Series?


The wxWidgets project has adopted the release model used by the Linux Kernel project where there are alternating sets of releases where one set are considered "stable" and the next set are considered "development." For wxWidgets "stable" and "development" do not refer to bugginess, but to the stability of the API and backwards compatibility.

 - **Stable**: For the duration of the series existing APIs are not modified, although new non-virtual class methods and such can be added. Binary compatibility of the C++ libs is maintained by not allowing any changes that modify the in-memory size or layout of the classes and structs. This can and often does impose limitations on what kinds of enhancements or bug fixes can be performed in a stable release series, however this really only affects the C++ layer because in Python being backwards compatible has a slightly different connotations.

 - **Development**: The main purpose of the development series of releases is to add new functionality or to correct problems that could not be corrected in a stable series because of binary compatibility issues, all in an effort to create the next stable series. So for the duration of the development series existing the APIs are allowed to be modified or removed as needed, although most of the time C++ source-level compatibility is maintained via deprecated overloaded functions or macros, etc. For wxPython this often means that there will be source-level incompatibilities because there is no overloading or macros, and in order to support the new version of the API sometimes the old version has to be removed. 

Because of the binary compatibility issues, the latest development version of wxWidgets/wxPython can often be less buggy than the latest version of the last stable release series. However there is the trade-off that the APIs may be changing or evolving between versions in the development series. 

**How do the version numbers work?**

For releases wxPython uses a 4 component version number. While this looks a lot like how version numbers are used in other Open Source projects, there are a few subtle differences. So for some release **A.B.C.D** you can deduce the following:

1. **Release Series**: The first two components of the version number (**A.B**) represent the release series, and if the **B** component is an even number then it is a stable series, if it is an odd number then it is an development release series. For example, 2.4, 2.6, and 2.8 are stable and the API is more or less frozen within each series, and 2.3, 2.5, and 2.7 are development and the API and functionality is allowed to change or evolve as needed.

Because of this there can be quite large changes between one stable series to the next (say 2.4 to 2.6) and this often throws people off because in other projects changes of that magnitute would have caused the first component of the version number to change. Instead you should think of the combination of **A.B** as being the major number of the version.

2. **Release Number**: The third component of the version number (C) represents one of the releases in a release series. For example, 2.5.0, 2.5.1, 2.5.2, 2.5.3... are all releases in the 2.5 release series. (And since in this case it is an development series then the API and functionality of 2.5.3 has evolved to be different in places than it was in 2.5.0.) The C++ wxWidgets releases usually stop here and only A.B.C releases are made.

3. Subrelease number, or wxPython release: The fourth component of the version number (D) is used to represent a subrelease, or incremental releases betweeen the official wxWidgets releases. These releases include fixes for wxWidgets bugs that wxPython may have exposed, or minor enhancements that are important for wxPython. This is not an arbitrary wxWidgets snapshot, but rather a tested version of the code with fixes and enhancements not yet available from wxWidgets except from the source code repository.

Source: https://wiki.wxpython.org/ReleaseSeries 

