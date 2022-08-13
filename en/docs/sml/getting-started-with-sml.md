---
title: "Getting started with sml"
slug: "getting-started-with-sml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
There is a dozen implementations of Standard ML. [MLton](http://mlton.org/) produces very optimized code, but has no [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop). [SML/NJ](http://smlnj.org/) is the most widely used, but has slightly difficult error messages for learning purposes. [Moscow ML](http://mosml.org/) and [Poly/ML](http://www.polyml.org/) are easy to get started with, but don't support the .mlb package format. That isn't essential for getting started, though.

Here are instructions for installing each of SML/NJ, Moscow ML and Poly/ML divided by operating system.

# On Windows

SML/NJ:
* Go to http://www.smlnj.org/dist/working/ and find the latest release, e.g. [110.80 Distribution Files](http://www.smlnj.org/dist/working/110.80/index.html).
* Scroll down and find the MS Windows Installer, e.g. [smlnj-110.80.msi](http://smlnj.cs.uchicago.edu/dist/working/110.80/smlnj-110.80.msi). Run the installer.
* You now have a REPL in e.g. `C:\Program Files (x86)\SML NJ\bin\sml.bat`.

Moscow ML:
* Go to http://mosml.org/ and click "Download Win. Installer". Run the installer.
* You now have a REPL in e.g. `C:\Program Files (x86)\mosml\bin\mosml.exe`.

# Using [Homebrew](http://brew.sh/) On MacOS

SML/NJ:
* Run `brew install smlnj` as your own user. Test REPL with `smlnj`.

Moscow ML:
* Go to http://mosml.org/ and click "Download PKG File". Run the installer.
* *Missing... Test REPL how? Is it in `$PATH` now?*

# On Ubuntu / Debian Linux
SML/NJ:
* Run `sudo apt-get install smlnj` as the super user. Test REPL with `smlnj`.

Moscow ML:
* *(Ubuntu)* Add the PPA as the super user. Test REPL with `mosml`.

      sudo add-apt-repository ppa:kflarsen/mosml
      sudo apt-get update
      sudo apt-get install mosml

# Adding readline support

In order to be able to use the arrow keys to navigate lines that were previously typed into the REPL, most of the SML compilers can benefit from the program `rlwrap`. Using Homebrew on MacOS, install this by `brew install rlwrap`, and on Ubuntu / Debian Linux, install this by `sudo apt-get install rlwrap`. Then in the terminal, try the following:

    alias mosml='rlwrap mosml -P full'
    alias sml='rlwrap sml'
    alias poly='rlwrap poly'

These aliases can be added to e.g. your `~/.bashrc` so they work by default.

The arrows key should now work better.

