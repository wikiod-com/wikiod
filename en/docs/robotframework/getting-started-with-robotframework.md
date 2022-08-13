---
title: "Getting started with robotframework"
slug: "getting-started-with-robotframework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Robot Framework 3.0 on a Windows Machine using Python 2.7.11
This is a quick guide to get Robot Framework 3.0 working on a Windows machine using Python 2.7.11 - It does not go into too much depth on the why and how, it simply gets you up and running. First things are first, let't go and install Python!

 1. Download [Python 2.7.11 for Windows][1]. (Windows x86-64 MSI installer or Windows x86 MSI installer depending on architecture)
 
 2. Run through the install, making sure you install "pip" and that you opt in for the "Add python.exe to Path" (You may have to restart your machine to take advantage of the Python PATH. In this guide, it presumes you don't have that luxury)

 3. Once it is installed, let's do a quick check to make sure it installed correctly. Run CMD as admin and navigate to where Python was installed to `cd C:\Python27` and type in `python -V`. It should return "Python 2.7.11"

That is it, Python is now installed to your machine. The next part is getting the Robot Framework Installed on your machine using pip.

 1. First, let's make sure we have the latest version of pip, by first navigating to the scripts directory within Python `cd C:\Python27\Scripts` and then entering `python -m pip install -U pip`. It should say that you have the most up to date version installed!

 2. Next, lets install Robot Framework by entering `pip install robotframework`

 3. Once pip has finished downloading and installing the files, enter `robot --version` to make sure it installed correctly. It should say Robot Framework 3.0 (Python 2.7.11 on win32/64)

 4. (Optional) If in the future there is an update for Robot Framework, you can run this command `pip install --upgrade robotframework`

  [1]: https://www.python.org/downloads/release/python-2711/

## Installation or Setup
Detailed instructions on getting Robot Framework set up or installed.

Robot framework  is a generic test automation framework.This is implemented using Python and is supported on Python 2 and Python 3 Jython (JVM) and IronPython (.NET) and PyPy. For 
 1. Acceptance testing  
 2. Acceptance test-driven development (ATDD)

**Prerequisites**
-------------

 1. Install a interpreters 
 2. Configuring PATH
 3. Setting https_proxy


Python has the most advanced implementations and it is suggested to use Python, if you do not have exceptional requirements. 


| Robot Framework Version| Supported interpreter Version|
| ------------------ | ------------------- |
| Robot Framework 3.0| Python 2.6  |
| Robot Framework 3.0| Python 2.7  |
| Robot Framework 3.0| Python 3.3  |
| Robot Framework 3.0| Jython 2.7 & Java 7 |
| Robot Framework 3.0| IronPython 2.7  |
| Robot Framework 2.5-2.8| Python 2.5  |
| Robot Framework 2.5-2.8| Jython 2.5  |
| Robot Framework 2.0-2.1| Python 2.3  |
| Robot Framework 2.0-2.1| Python 2.4  |
| Robot Framework 2.0-2.1| Jython 2.2  |

**Python installation**
-----------------------

Desired version of python can be downloaded from https://www.python.org/downloads/

**Jython installation**
-------------------
An installer can be found at http://jython.org. You can run this executable JAR package from the command line like javaava -jar jython_installer-<version>.jar. 


**IronPython installation**
-------------------
An installer can be found at  http://ironpython.net/download/ for IronPython 2.7.When using IronPython, an additional dependency is installing [elementtree][1] module 1.2.7

**Configuring PATH & Setting https_proxy**
-------------------
Add Python installation directory (by default C:\Python27\, C:\Python27\Scripts, C:\jython2.7.0\bin etc on windows )  and Scripts directory to the beginning of your path variable

Value of https_proxy should be the URL of the proxy. This is required when these packages are installed with pip and you are in a proxy network

**Installing Robot Framework with pip**
-------------------

Install the latest version of robotframework

    pip install robotframework

Install a specific version

    pip install robotframework==2.0

**Installing Robot Framework from source**
-------------------
Source distribution of Robot Framework can be found at https://code.google.com/archive/p/robotframework/downloads.Robot Framework is installed from source using Python's standard setup.py script in the source scripts directory

    python setup.py install
    jython setup.py install
    ipy setup.py install

  [1]: http://effbot.org/downloads/#elementtree

