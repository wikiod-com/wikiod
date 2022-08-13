---
title: "Getting started with scrapy"
slug: "getting-started-with-scrapy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Scrapy
prerequisite of scrapy installation:

 - Python 2.7 or above 3.3
 - pip and setuptools Python packages.
 - lxml
 - OpenSSL.

You can install Scrapy using pip. To install using `pip` run:

    pip install Scrapy

**  **
**Platform specific installation**
**  **
**Anaconda**

> *This is the recommended way to install Scrapy.*

If you already have installed Anaconda or Miniconda, the company Scrapinghub maintains official conda packages for Linux, Windows and OS X.

To install Scrapy using conda, run:

    conda install -c scrapinghub scrapy


**  **
**Ubuntu 9.10 or above**

Use the official [Ubuntu Packages][1], which already solve all dependencies for you and are continuously updated with the latest bug fixes.

If you prefer to build the python dependencies locally instead of relying on system packages you’ll need to install their required non-python dependencies first:

    sudo apt-get install python-dev python-pip libxml2-dev libxslt1-dev zlib1g-dev libffi-dev libssl-dev

You can install Scrapy with `pip` after that:

    pip install Scrapy

**  **
**Archlinux**

You can follow the generic instructions or install Scrapy from AUR Scrapy package:

    yaourt -S scrapy

**  **
**Windows**

> Scrapy with Python 3 is not yet supported on Windows.

Follow This steps to install scrapy on windows:

 - Install Python 2.7
 - adjust PATH environment variable to include paths to the Python executable and additional scripts. The following paths need to be added to PATH:

    > C:\Python27\;C:\Python27\Scripts\;

 - Install pywin32 from [here][2]

 - let’s install Scrapy:

        pip install Scrapy

**  **
**Mac OS X**

Building Scrapy’s dependencies requires the presence of a C compiler and development headers. On OS X this is typically provided by Apple’s Xcode development tools. To install the Xcode command line tools open a terminal window and run:

    xcode-select --install

There’s a [known issue][3] that prevents `pip` from updating system packages. This has to be addressed to successfully install Scrapy and its dependencies. Here are some proposed solutions:

 - (Recommended) Don’t use system python, install a new, updated version that doesn’t conflict with the rest of your system. Here’s how to do it using the homebrew package manager:
     - Install homebrew following the instructions in http://brew.sh/
     - Update your `PATH` variable to state that homebrew packages should be used before system packages (Change `.bashrc` to `.zshrc` accordantly if you’re using [zsh][4] as default shell):

           echo "export PATH=/usr/local/bin:/usr/local/sbin:$PATH" >> ~/.bashrc

     - Reload `.bashrc` to ensure the changes have taken place:

           source ~/.bashrc

    - Install python:

          brew install python

    - Latest versions of python have `pip` bundled with them so you won’t need to install it separately. If this is not the case, upgrade python:

          brew update; brew upgrade python

 - (Optional) Install Scrapy inside an isolated python environment.

   This method is a workaround for the above OS X issue, but it’s an overall good practice for managing dependencies and can complement the first method.

   [virtualenv][5] is a tool you can use to create virtual environments in python. We recommended reading a tutorial like http://docs.python-guide.org/en/latest/dev/virtualenvs/ to get started.


After any of these workarounds you should be able to install Scrapy:

    pip install Scrapy


  [1]: http://doc.scrapy.org/en/latest/topics/ubuntu.html#topics-ubuntu
  [2]: https://sourceforge.net/projects/pywin32/
  [3]: https://github.com/pypa/pip/issues/2468
  [4]: http://www.zsh.org/
  [5]: https://virtualenv.pypa.io/en/latest/

## Creating a project
Before starting work with scrapy you have to start a project where you want to store your code. Enter the directory and run this code

    scrapy startproject helloProject

The third part of this code is project name. This code will create a "helloProject" directory with the following contents:

    helloProject/
        scrapy.cfg            # deploy configuration file

        helloProject/         # project's Python module, you'll import your code from here
            __init__.py

            items.py          # project items file

            pipelines.py      # project pipelines file

            settings.py       # project settings file

            spiders/          # a directory where you'll later put your spiders
                __init__.py



