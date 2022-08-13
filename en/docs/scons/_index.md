---
title : SCons Tutorial
slug : scons-tutorial
weight : 9985
draft : false
images : []
type : docs
---

SCons is a build system. It takes a bunch of input files and run tools on them to produce output. SCons is written in pure Python, works the same way on Linux, Windows and OS X, and may be run without installation.

SCons' `SConstruct` files are Python scripts with built-in commands that create a `build tree`. SCons executes build process in phases. First is reading files and constructing a build tree. Second is traversing the tree to build target files.

