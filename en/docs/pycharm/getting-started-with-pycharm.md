---
title: "Getting started with PyCharm"
slug: "getting-started-with-pycharm"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
There are two PyCharm editions: **Community** and **Professional**.<br> 
Both are downloadable from [JetBrains website][1]. Additionally, there is another edition, [PyCharm Edu][2]. JetBrains recommend this edition, if you are learning or teaching Programming with Python.

The supported platforms are **Windows**, **Linux** and **macOS**.

To install PyCharm on:

 - Windows: Run the .exe file and follow the instructions of PyCharm Setup wizard
 - Linux: Unpack the .tar.gz archive into any directory within your home directory
 - OS X: Open the .dmg package, and drag PyCharm to the Applications folder

**Windows**

Go to the download page, choose the desired edition and go through the classic installer.

**Linux**

There are two options on how to install PyCharm on Linux which depends on if your distro supports ppa packages.

1. If your distro doesn't support ppa packages or simply you want download PyCharm directly to the JetBrains website, follow this procedure:
     - Download the `<pycharm-professional or pycharm-community>-*.tar.gz` file from the download page. 
     - Unpack the `<pycharm-professional or pycharm-community>-*.tar.gz` file to a different folder, if your current "Download" folder doesn't support file execution
         
           tar xfz <pycharm-professional or pycharm-community>-*.tar.gz <new_archive_folder> 

     - Switch to the bin directory:<br>
         
           cd <new archive folder>/<pycharm-professional or pycharm-community>-*/bin

     - Run `pycharm.sh` from the `bin` subdirectory

2. If your distro supports ppa packages, type this in the terminal:

       sudo add-apt-repository ppa:mystic-mirage/pycharm
       sudo apt-get update
       sudo apt-get install pycharm

If you are not sure about this information, choose the first method. 

**macOS**

Go to the download page, choose the desired edition and open the .dmg package, and drag PyCharm to the Applications folder.


  [1]: https://www.jetbrains.com/pycharm/download/
  [2]: https://www.jetbrains.com/pycharm-edu/

## Hello World
1. Open PyCharm
1. Select *Create New Project*
    - Select the desired location to create the project
    - Select the python interpreter
    - Click *Create*
1. Create a new python file e.g. with File->New...-> Python File
1. Add the following code

    ```
    #!/usr/bin/env python
    print("Hello World")
    ```
    
1. Run the code e.g. with Run->Run...->*YourCreatedPytonFile*

