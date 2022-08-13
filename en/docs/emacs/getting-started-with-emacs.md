---
title: "Getting started with emacs"
slug: "getting-started-with-emacs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting emacs set up or installed.

Official instructions are available [on the GNU Emacs website](https://www.gnu.org/software/emacs/download.html).

# Debian systems

On systems with the Debian package manager (such as Debian, Ubuntu, and Mint) Emacs can be installed via the simple command:

    sudo apt-get install emacs

For a bleeding-edge release one can use the following ppa:

    sudo apt-add-repository ppa:ubuntu-elisp/ppa
    sudo apt-get install emacs-snapshot

## Build for source

If your debian based distro does not have the version of emacs you want you can build it from scratch.

    sudo apt-get build-dep emacs24 -y
    
    cd /tmp/
    
    wget http://alpha.gnu.org/gnu/emacs/pretest/emacs-25.0.93.tar.xz
    tar -xvf emacs-25.0.93.tar.xz
    
    cd emacs-25.0.93
    ./configure
    make
    sudo make install
    
    rm -rf /tmp/emacs-25.0.93*

# Redhat systems

On systems with the Redhat package manager (such as RHEL, CentOS, and Fedora Core) Emacs can be installed via the simple command:

    sudo yum install emacs

# Arch Linux

Emacs can be installed via the simple command:

    sudo pacman -Syu emacs

# Gentoo and Funtoo

On systems running Portage, Emacs can be installed via the simple command:

    sudo emerge emacs

# GSRC (GNU Source Release Collection)

Works on any GNU/Linux system for getting the latest version of emacs without using the system package manager (which may be out of date) or downloading the archive or binary. To install `gsrc` see it's [documentation][1]. Then:

    cd gsrc
    make -C gnu/emacs install
    # add the binaries to your PATH
    source ./setup.sh


# Darwin systems

## Homebrew

<!-- language: lang-sh -->

    brew install emacs --with-cocoa # basic install
    # additional flags of interest can be viewed by calling `brew info emacs`
    brew linkapps emacs # to put a symlink in your Applications directory

## MacPorts

    sudo port install emacs

## pkgsrc
    sudo pkgin -y install emacs-24.5

## App Bundle
Precompiled app bundles for the latest stable and development versions can be downloaded at https://emacsformacosx.com.

# Windows

## [Chocolatey](https://chocolatey.org/install) package manager

Emacs can be installed with

    choco install emacs

## [Scoop](http://scoop.sh) package manager

Can be installed from extras bucket

    scoop bucket add extras
    scoop install emacs

## Official Binary Installers 

* [stable](http://ftpmirror.gnu.org/emacs/windows)
* [latest](http://sourceforge.net/projects/emacs-bin/files/snapshots)

(Note that official binaries do not come with some libraries - e.g., libraries for image formats)

## Other Binary Installers

* [Emacs with pre-compilled AUCTeX and ESS](http://vgoulet.act.ulaval.ca/en/emacs) 
* [64-Bit GNU Emacs for MS Windows with optimization][2] provides native and optimized 64-bit binary installer with unmodified source code from git master and release version, with JPEG, GIF, PNG, TIFF, SVG, XML2, and GnuTLS support out-of-box.

  [1]: https://www.gnu.org/software/gsrc/
  [2]: http://emacsbinw64.sourceforge.net/ "64-Bit GNU Emacs for MS Windows with optimization"


## Interactive Emacs Tutorial
From within Emacs, type `C-h t` (Control-h, t) to get an excellent interactive tutorial within Emacs.  The user learns basic navigation and editing by operating on the TUTORIAL text itself, as they read the tutorial.  (Modifications to the tutorial are discarded when the tutorial is closed, so each time a user requests the tutorial, it's a clean default version of the tutorial.

Helpfully, the first thing in the tutorial is how to understand `C-<chr>` and `M-<chr>` references in the text.  The second thing is how to page forward and backwards in the text.

## Emacs Rocks Video Tutorials
Good video tutorials about Emacs can be found at [emacsrocks.com][1].

[![enter image description here][2]][2]


  [1]: http://emacsrocks.com
  [2]: https://i.stack.imgur.com/dl7L0.png

