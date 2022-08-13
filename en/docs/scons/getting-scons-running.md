---
title: "Getting SCons running"
slug: "getting-scons-running"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

SCons is written in Python 2 and doesn't need any dependencies to work. You can just copy its scripts to your project source tree and run from here. Or you may want to use version packaged for your operating system.

## Installing on Linux
On Debian or Ubuntu, you can install SCons using

    $ sudo apt-get install scons


-------------

On [YUM](https://en.wikipedia.org/wiki/Yellowdog_Updater,_Modified)-based systems, use

    $ sudo yum install scons

-------------

You can install using an [RPM](https://en.wikipedia.org/wiki/RPM_Package_Manager) by downloading it, then running

    $ sudo rpm -Uvh http://prdownloads.sourceforge.net/scons/scons-2.5.0-1.noarch.rpm    

    



## Installing on Windows
Grab installer from http://scons.org/pages/download.html

Or try `pip` installation tool that comes with Python:

    pip install scons

If `scons` still can't be found after that, make sure that Python `Scripts/` folder is added to `PATH` for your Python installation.

## Running from source
If you have modifications to share or just want to try new version in development.

    $ hg clone https://bitbucket.org/scons/scons
    $ python scons/src/script/scons.py

## Installing with Python pip
    pip install scons

If you are not to run `scons` from command line, check that Python scripts directory is added to PATH for your installation.

If you want to play with API, `import SCons` from Python won't work, because SCons 2.5.x and below allows to install multiple versions side-by-side. This was needed to switch between different SCons versions during development and troubleshooting. Now the more common way for this is to use `virtualenv` or just [run it from source](https://www.wikiod.com/scons/getting-scons-running#Running from source).

