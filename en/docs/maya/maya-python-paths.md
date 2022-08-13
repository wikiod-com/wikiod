---
title: "Maya Python Paths"
slug: "maya-python-paths"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

This page should cover various ways to set up Maya python paths - userSetup, maya.env, environment variables and so on

## Using userSetup.py
Add arbitrary paths to the Maya Python environment in the `userSetup.py` file.   `userSetup.py` is a Python file (**not** a module) which gets automatically executed on Maya startup. `userSetup.py` can live in a number of locations, depending on the os and environment variables.

When Maya starts, it will execute the contents of the userSetup file. Adding Python paths here will allow it to find modules:

     import sys
     sys.path.append("/path/to/my/modules")

This will make Python module files in '/path/to/my/modules' available for import using the standard `import` directive.

For more advanced setups, the `site` module can do the same using the `addsitedir()` function.  `site.addsitedir()` supports [.pth files](http://bob.ippoli.to/archives/2005/02/06/using-pth-files-for-python-development/) which  configures multiple paths in one go. 

For example, three folders of unrelated Python could be arranged like this:

      python_files
      | 
      +---- studio
      |      +  module1.py
      |      +  module2.py
      |     
      +---- external
             |
             +---- paid
             |      + paidmodule.py
             |
             +---- foss
                    + freemodule.py

Using `sys.path` directly you'd have to add `python_files/studio`, `python_files/external/paid` and `python_files/external/paid` manually.  However you could add a .pth file to the root of `python_files` that looked like this:

      studio
      external/paid
      external/foss

and call this in userSetup:

     import site
     site.addsitedir("/path/to/python_files")

and you'll get all of the paths in one go.

## Using Environment Variables
The Maya Python interpreter works like a regular Python intepreter, so it will use the same environment variables to find importable files as any other Python 2.6 or 2.7 installation (described in more detail in the [Python documentation](https://docs.python.org/2/using/cmdline.html#using-on-envvars).  

If there is no other python installation on your machine you can use the environment variables to point at the location of your Python files for Maya (if you do have another Python, changing these for Maya's sake may interfere with your other Python installation - you'd be better off using a userSetup or startup script).  Set variable `PYTHONPATH` so it includes your search paths.  If you're editing the variable to include multiple paths remember that on *NIX systems the paths are separated by colons:

      export PYTHONPATH="/usr/me/maya/shared:/usr/me/other_python"

where on Windows they are semicolons:

      setx  PYTHONPATH C:/users/me/maya;//server/shared/maya_python



Multiple configurations
---------------------
One advantage of using environment variables is that you can quickly re-configure a maya install to load tools and scripts from different locations for different projects.  The easiest way to do this is to set the `PYTHONPATH` right before launching Maya so that you inherit the necessary paths for this maya session.  For example

      set PYTHONPATH=C:/users/me/maya;//server/shared/maya_python
      maya.exe

will launch Maya (on Windows) with the paths `C:/users/me/maya` and `//server/shared/maya_python` available for use.  You could launch a second copy of Maya from a new commandline using a different `set` command and the second Maya would use different paths.

Because it's hard for most end-users to type these kinds of things, it's a good idea to automate the process with a batch or shell file that sets the local environment variables and launches maya.  _note: we need examples of this for .bat and .sh files_  In this system you'd distribute a .bat or .sh file for each project you were supporting and your users would launch maya using those;  launching maya without the bat file would revert them to the default Maya configuration without any custom scripts.



