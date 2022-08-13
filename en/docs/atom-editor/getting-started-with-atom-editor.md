---
title: "Getting started with atom-editor"
slug: "getting-started-with-atom-editor"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is Atom?
Atom is a hackable text editor created by GitHub and developed on top of the Electron desktop application platform.

This means it can be used as a text editor for basic programming up to a full-sized IDE. It is also extremely customisable, it provides thousands of community-made packages (syntax highlighting, improved UX, etc.) and themes to suit everyone's needs. It is also available on Windows, MacOS, and Linux.

Here is an example:

[![Example of the Atom editor][1]][1]

Atom provides other helpful features including:
 - Opening directories
 - Multiple editing tabs
 - Side-by-side editing panes
 - Multiple editing cursors
 - Line switching
 - File and directory tree management


  [1]: https://i.stack.imgur.com/O8jVy.png

## Running a "Hello, World!" program in Python using Atom from scratch
Atom is versatile and flexible text editor and has hundreds of community-made, open-source packages that can compile and run source files, for many languages. This guide will show how easy it is to code Python with the Atom editor.

This guide assumes you do not have Python nor Atom installed in your system.

Step 1: Installing Python
-------------------------

Python can be installed from the either [the official website](https://www.python.org/), or if you're using Linux, through package managers (however Python usually comes pre-installed anyways).

If you're a Windows user, do not forget to set `python.exe` to your `%PATH%`.

Step 2: Installing Atom
-----------------------

You can install the Atom editor from [the official website](https://atom.io/) or through package managers.

Step 3: Configuring Atom
-----------------------

For more information about installing packages, and themes, read [this dedicated topic](https://www.wikiod.com/atom-editor/themes-and-packages).

In order to compile and run programs, the Atom community provides packages to fill that need. For this example, we will be using [`script`](https://atom.io/packages/script) to run our program.

Go to File > Settings > Install. 

Type `script` in the search bar and install it. When it is installed, it should be listed in "Packages" in the Settings bar. It should be noted that `script` is not capable of user input.

If you're using MacOS or Linux, you can use the `apm` package manager to install packages.

Step 4: Programming and executing
-----------------------

Pick a directory where you would like to store your PY source file. 

1. Make sure you can see the Tree View pane; if you cannot see this pane, you can toggle it by going to View > Toggle Tree View.
2. Go to File > Add Project Folder and select a directory which will be set as your root directory for a project.
3. Right-click the folder and click New File, then enter in `hello-world.py` and type in the following code:

       print("Hello, World!")

4. Press <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>B</kbd> to run the script. Alternatively, you can go to View > Toggle Command Palette and enter `Script: Run`.

   The script should return:

       Hello, World!
       [Finished in 0.125s] 

