---
title: "Set up the FreeBSD development environment"
slug: "set-up-the-freebsd-development-environment"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## ctags
**ctags** is a useful utility you can use to read and move around the source code more efficiently. The built-in **ctags(1)** however is not the Exuberant Ctags utility you might expect. 

You can install Exuberant Ctags (**exctags(1)**) using either ports or `pkg`:

## Build `exctags(1)` using ports

    cd /usr/ports/devel/ctags/ && make install clean

## Download and install a prebuilt binary of Exuberant Ctags:

    pkg install ctags

# Create the tag file

    exctags -R

