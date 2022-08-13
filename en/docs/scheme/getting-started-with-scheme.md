---
title: "Getting started with scheme"
slug: "getting-started-with-scheme"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing CHICKEN Scheme
[CHICKEN](https://call-cc.org) is a Scheme interpreter and compiler with its own extension module system called "eggs".  It is capable of compiling Scheme to native code by first compiling Scheme to C.

# Installing

## Debian or Ubuntu or other derived distros:

    sudo apt-get install chicken-bin

## Fedora / RHEL / CentOS:

    sudo yum install chicken-bin

## Arch Linux:

    sudo pacman -S chicken

## Gentoo:

    sudo emerge -av dev-scheme/chicken

## OS X with Homebrew:

    brew install chicken

## OpenBSD
    doas pkg_add -vi chicken

##  Microsoft Windows

- Install [MSYS2](https://msys2.github.io)
- Run the MSYS2 MinGW-w64 Shell
- Install some prerequesites by running:

      pacman -S mingw-w64-cross-toolchain base-devel mingw-w64-x86_64-gcc winpty wget

- Download the [latest release tarball](https://code.call-cc.org/releases/current/) by typing:

      wget https://code.call-cc.org/releases/current/chicken.tar.gz

- Extract the tarball by running `tar xvf chicken.tar.gz`
- Enter the extracted directory, for example by typing `cd chicken-4.11.0`
- Run `make PLATFORM=mingw-msys install`

*If you have trouble running `csi`, try instead running `winpty csi`*

# Using CHICKEN

To use the CHICKEN Scheme REPL, type `csi` at the command line.

To compile a Scheme program using CHICKEN, run `csc program.scm`, which will create an executable named `program` in the current directory.

## Installing modules

Chicken Scheme has a lot of modules that can be browsed [in the egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html). Eggs are scheme modules that will be downloaded and then compiled by chicken-scheme. In some cases, it might be necessary to install external dependencies using your usual package manager.

You install the chosen eggs with this command:

    sudo chicken-install [name of egg]

## Making use of the REPL

You may wish to add `readline` support to your REPL to make line editing in `csi` behave more like you might expect.

To do this, run `sudo chicken-install readline`, and then create a file named `~/.csirc` with the following contents:

    (use readline)
    (current-input-port (make-readline-port))
    (install-history-file #f "/.csi.history")



## Installing mit-scheme
The following are examples of how to install [MIT/GNU Scheme][1]:

**Debian/Ubuntu installation:**

    sudo apt-get install mit-scheme

**Manual installation:**

Download the Unix binary directly from the [GNU Project][1], then follow the instructions from [the official webpage][2]:  

    # Unpack the tar file
    tar xzf mit-scheme.tar.gz

    # move into the directory
    cd mit-scheme/src

    # configure the software
    ./configure

By default, the software will be installed in `/usr/local`, in the subdirectories bin and lib. If you want it installed somewhere else, for example `/opt/mit-scheme`, pass the `--prefix` option to the configure script, as in `./configure --prefix=/opt/mit-scheme`.

The configure script accepts all the normal arguments for such scripts, and additionally accepts some that are specific to MIT/GNU Scheme. To see all the possible arguments and their meanings, run the command `./configure --help`. 

    # build
    make compile-microcode

    # compile
    make install # may require super-user permissions (Depending on configuration)

**Windows 7**:

The self-installing [executable][3] can be found in the [official website][3].

>MIT/GNU Scheme is distributed as a self-installing executable. Installation of the software is straightforward. Simply execute the downloaded file and answer the installer's questions. The installer will allow you to choose the directory in which MIT/GNU Scheme is to be installed, and the name of the folder in which the shortcuts are to be placed. 


  [1]: https://www.gnu.org/software/mit-scheme/
  [2]: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-user/Unix-Installation.html
  [3]: http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/9.2/mit-scheme-9.2-i386-win32.exe

