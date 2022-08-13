---
title: "Getting started with tkinter"
slug: "getting-started-with-tkinter"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Tkinter comes pre-installed with the Python installer binaries for Mac OS X and the Windows platform. So if you install Python from the [official binaries][1] for Mac OS X or Windows platform, you are good to go with Tkinter.

For Debian versions of Linux you have to install it manually by using the following commands.

**For Python 3**
> sudo apt-get install python3-tk

**For Python 2.7**
> sudo apt-get install python-tk


Linux distros with yum installer can install tkinter module using the command:

> yum install tkinter

**Verifying Installation**

To verify if you have successfully installed Tkinter, open your Python console and type the following command:
    
    import tkinter as tk # for Python 3 version

or
 
    import Tkinter as tk # for Python 2.x version

You have successfully installed Tkinter, if the above command executes without an error.

To check the Tkinter version, type the following commands in your Python REPL:


For python 3.X

    import tkinter as tk
    tk._test()

For python 2.X

    import Tkinter as tk
    tk._test()

Note: Importing `Tkinter as tk` is not required but is good practice as it helps keep things consistent between version.

  [1]: https://www.python.org/downloads/
  [2]: http://www.tcl.tk/software/tcltk/download.html

## Hello, World! (modular, object-oriented)
    import tkinter as tk
    
    class HelloWorld(tk.Frame):
        def __init__(self, parent):
            super(HelloWorld, self).__init__(parent)
    
            self.label = tk.Label(self, text="Hello, World!")
            self.label.pack(padx=20, pady=20)
            
    if __name__ == "__main__":
        root = tk.Tk()
    
        main = HelloWorld(root)
        main.pack(fill="both", expand=True)
    
        root.mainloop()

----

Note: It's possible to inherit from just about any tkinter widget, including
the root window. Inheriting from `tkinter.Frame` is at least arguably
the most flexible in that it supports multiple document interfaces (MDI),
single document interfaces (SDI), single page applications, and multiple-page
applications.


## Hello, World! (minimal)
Let's test our basic knowledge of tkinter by creating the classic "Hello, World!" program.

First, we must import tkinter, this will vary based on version (see remarks section about "Differences between Python 2 and 3")

In Python 3 the module `tkinter` has a lowercase t:

    import tkinter as tk

In Python 2 the module `Tkinter` has a uppercase T:

    import Tkinter as tk 

Using `as tk` isn't strictly necessary but we will use it so the rest of this example will work the same for both version.

now that we have the tkinter module imported we can create the root of our application using the `Tk` class:

    root = tk.Tk()

This will act as the window for our application. (note that _additional_ windows should be `Toplevel` instances instead)

Now that we have a window, let's add text to it with a `Label`
    
    label = tk.Label(root, text="Hello World!") # Create a text label
    label.pack(padx=20, pady=20) # Pack it into the window

Once the application is ready we can start it (enter the _main_ event _loop_) with the `mainloop` method

    root.mainloop()

This will open and run the application until it is stopped by the window being closed or calling exiting functions from callbacks (discussed later) such as `root.destroy()`.

Putting it all together:

    import tkinter as tk # Python 3.x Version
    #import Tkinter as tk # Python 2.x Version

    root = tk.Tk()

    label = tk.Label(root, text="Hello World!") # Create a text label
    label.pack(padx=20, pady=20) # Pack it into the window

    root.mainloop()

And something like this should pop up:


[![tkinter window][2]][2]


  [1]: https://www.wikiod.com/tkinter/multiple-windows-toplevel-widgets
  [2]: http://i.stack.imgur.com/DreFs.png

