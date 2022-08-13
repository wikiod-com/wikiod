---
title: "Getting started with composer-php"
slug: "getting-started-with-composer-php"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Composer on Ubuntu
Before we download and install Composer, we need to make sure our server has all dependencies installed.

First, update the package manager cache by running:

    sudo apt-get update

Now, let's install the dependencies. We'll need `curl` in order to download Composer and `php5-cli` for installing and running it. `git` is used by Composer for downloading project dependencies. Everything can be installed with the following command:

    sudo apt-get install curl php5-cli git

Now let's install it:

    curl -sS https://getcomposer.org/installer | sudo php -- --install-dir=/usr/local/bin --filename=composer

This will download and install Composer as a system-wide command named composer, under /usr/local/bin. The output should look like this:

    Output
    #!/usr/bin/env php
    All settings correct for using Composer
    Downloading...
    
    Composer successfully installed to: /usr/local/bin/composer
    Use it: php /usr/local/bin/composer

To test your installation, run:

    composer

And you should get output similar to this:

    Output
       ______
      / ____/___  ____ ___  ____  ____  ________  _____
     / /   / __ \/ __ `__ \/ __ \/ __ \/ ___/ _ \/ ___/
    / /___/ /_/ / / / / / / /_/ / /_/ (__  )  __/ /
    \____/\____/_/ /_/ /_/ .___/\____/____/\___/_/
                        /_/
    Composer version 1.0-dev (9859859f1082d94e546aa75746867df127aa0d9e) 2015-08-17 14:57:00
    
    Usage:
     command [options] [arguments]
    
    Options:
     --help (-h)           Display this help message
     --quiet (-q)          Do not output any message
     --verbose (-v|vv|vvv) Increase the verbosity of messages: 1 for normal output, 2 for more verbose output and 3 for debug
     --version (-V)        Display this application version
     --ansi                Force ANSI output
     --no-ansi             Disable ANSI output
     --no-interaction (-n) Do not ask any interactive question
     --profile             Display timing and memory usage information
     --working-dir (-d)    If specified, use the given directory as working directory.
    
    ....

## Installing on Windows
Here we will simply use the installer.

This is the easiest way to get Composer set up on your machine.

Download and run [Composer-Setup.exe][1]. It will install the latest composer version and set up your `PATH` so that you can just call `composer` from any directory in your command line.

**Note**: Close your current terminal. Test usage with a new terminal: This is important since the PATH only gets loaded when the terminal starts.

**Note-2**: Set up `PATH` in windows 10 
1. Right click on start up(windows logo)->`system ->Advance system settings->Environment variables->System variables[below box] ->`select `Path` and click `Edit`
2. Click New and add this value `C:\ProgramData\ComposerSetup\bin`
3. Now open your terminal [cmd] and test `composer --version` 

  [1]: https://getcomposer.org/Composer-Setup.exe

## Overview
Composer is a tool for dependency management in PHP. It allows you to declare the libraries your project depends on and it will manage (install/update) them for you.

Composer is not a package manager in the same sense as Yum or Apt are. Yes, it deals with "packages" or libraries, but it manages them on a per-project basis, installing them in a directory (e.g. vendor) inside your project.

Composer requires PHP 5.3.2+ to run. A few sensitive php settings and compile flags are also required, but when using the installer you will be warned about any incompatibilities.

To install packages from sources instead of simple zip archives, you will need git, svn, fossil or hg depending on how the package is version-controlled.

