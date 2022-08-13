---
title : wxpython Tutorial
slug : wxpython-tutorial
weight : 9949
draft : false
images : []
type : docs
---

What Is wxPython
---
Simply put wxPython is a set of bindings to the **[wxWidgets][1]** C++ Cross Platform GUI library.

Ok what is wxWidgets
====
The wxWidgets library provides a free, gratis & open source, set of abstractions for the various GUI elements so that the native controls are still used, where available, maintaining the native look, feel & speed. As such it provides an abstraction for GUI creation and a number of other utilities in a platform that lets developers create applications for Windows, Mac OS X, Linux and other platforms using a single code base. wxWidgets was started in 1992 and you can see a detailed history [here][2]. The wxWidgets library is distributed under the wxWindows License, which is based on the **L-GPL but with an exception clause**. *The exception clause allows you to link your application either dynamically or statically to wxWidgets **without** the requirement to distribute the source for your your own application. In other words, you can use wxWidgets for either **free or commercial** projects, at **no cost**. The license encourages you to give back enhancements you make to the wxWidgets library itself.*

The highlights, *note that wxWidgets comprises 100s of classes for cross platform application develepment*:

 - Window Layout Using Sizers
 - Device Contexts (along with pens, brushes and fonts)
 - Comprehensive Event Handling System
 - HTML Help Viewer
 - Sound and Video Playback
 - Unicode and Internationalization Support
 - Document/View Architecture
 - Printing Archiecture
 - Sockets
 - Multithreading
 - File and Directory Manipulation
 - Online and Context-Sensitive Help
 - HTML Rendering
 - Basic Containers
 - Image Loading, Saving, Drawing and Manipulation
 - Date-Time Library and Timers
 - Error Handling
 - Clipboard and Drag-and-Drop

Note that some of these facilities, *e.g. threading,* are not actually GUI related but provide a useful cross platform abstraction so that, in the case of threading for example, one set of application code will work on any supported platform.

For many years the wxWidgets library, produced 4 separate builds, *in addition to debug builds* from one set of source code, static and dynamic libraries built for both ASCII and Unicode. It is usually available pre-built in the most common variants and as source code to build *with the various options* for the target environment and with the developers C++ tool chain with numerous tool chains being supported.

The python bindings for this library and some additions form wxPython.

Back to What Is wxPython, (what does it give me)?
===

wxPython gives a developer a way of benefiting from a cross platform GUI library, with a clear licence, while also giving the benefits of Python. Like wxWidgets and Python wxPython is free, gratis & open source, and available for use and distribution in both free and commercial projects *without a resulting requirement to distribute your source code*.

 - Full GUI Suite including, (but not limited to):
   - Windows (including MDI Windows)
   - Wizards
   - Frames & MiniFrames
   - Dialogues, Standard, Advanced & Custom
   - Books, Trees, Grids & Data View Controls
   - Gauges, Sliders, Spinners, Animations, Clipboard, Drag & Drop
   - HTML, PDF & Image viewer support
   - GUI components can be absolutely positioned but it is strongly recommended to use sizer based layout which support auto sizing, etc.
 - Cross Platform - Support GUIs for Windows, OS-X & Linux with a single code base *without conditional statements in your code*
 - Native speed, look & feel.
 - Rapid prototype, test & debug - *remember that this is python*
 - Run & edit samples of just about everything in the demo package.
 - Clear licence for gratis use even in commercial products.
 - If necessary your python GUI can be refactored to a C++ wxWidgets GUI later as it is already using it.
 - Large, active & helpful user & developer community both on [StackOverflow][14] and [mailing lists][15].

Note that where python itself provides a cross platform mechanism for implementing the utility functions of wxWidgets, *threading again being a good example*, it is **intentionally** omitted from wxPython.

wxPython also has a very large suite of demonstrations that can be run, tested and edited from within the Documents and Demo package.

Flavours of wxPython
---

**ASCII vs Unicode**:
===
For many years, *as with wxWidgets*, developers had to choose between ASCII and Unicode builds as well as needing a build for their specific version of python as well as the 32/64 bit options. As of about wxPython 2.8.9 the ASCII only build of wxPython has been dropped so Unicode support is always available.

**Classic vs. Phoenix**:
===
Since wxPython 3.0.0 there have existed the *released* "Classic" build of wxPython and a Phoenix *currently unreleased* build.  The classic build tends to lag behind the wxWidgets builds of the same numbers and the documentation package is the C++ - it is available for download for various platforms, (see [Installation of Classic][3]), in the case of windows as an executable installer. The Phoenix bindings, being largely automatically generated, should follow more closely on the wxWidgets builds and also include wxPython specific documentation - it is build-able from source or nightly builds *as wheels* can be obtained using **pip**, (see [Installation of Phoenix][4]).

In wxPython but not wxWidgets
===

wxPython extends the wxWidgets library with a number of features, *the following are just a few,* that are not available in wxWidgets:

 - Programmers Editors & Shells: [crust][5], [crustslices][6], [AlaCart & AlaMode][7], [AlaModeTest][8]
 - [Interpreter][9] & [magic][10] 
 - Inspection - this allows you to launch a window to browse all of your applications GUI components.
 - An extensive set of Demos

Demo Screenshots *on Win10*
---
The wxPython demo with all the branches closed:
[![enter image description here][11]][11]
One of the recent additions:
[![enter image description here][12]][12]
One of the AGW, (Advanced Generic Widgets):
[![enter image description here][13]][13]


  [1]: https://www.wxwidgets.org/
  [2]: https://www.wxwidgets.org/about/history/
  [3]: https://www.wikiod.com/wxpython/getting-started-with-wxpython#Installation of wxPython Classic
  [4]: https://www.wikiod.com/wxpython/getting-started-with-wxpython#Installation of wxPython Phoenix
  [5]: https://wxpython.org/Phoenix/docs/html/wx.py.crust.html#module-wx.py.crust
  [6]: https://wxpython.org/Phoenix/docs/html/wx.py.crustslices.html#module-wx.py.crustslices
  [7]: https://wxpython.org/Phoenix/docs/html/wx.py.editor.html#module-wx.py.editor
  [8]: https://wxpython.org/Phoenix/docs/html/wx.py.PyAlaModeTest.html#module-wx.py.PyAlaModeTest
  [9]: https://wxpython.org/Phoenix/docs/html/wx.py.interpreter.html#module-wx.py.interpreter
  [10]: https://wxpython.org/Phoenix/docs/html/wx.py.magic.html#module-wx.py.magic
  [11]: http://i.stack.imgur.com/cGeIw.png
  [12]: http://i.stack.imgur.com/KQ68l.png
  [13]: http://i.stack.imgur.com/OlT2K.png
  [14]: http://stackoverflow.com/questions/tagged/wxpython
  [15]: https://wxpython.org/maillist.php

