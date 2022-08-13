---
title: "The os Module"
slug: "the-os-module"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

This module provides a portable way of using operating system dependent functionality.

## Syntax
 - import os

## Parameters
| Parameter | Details |
| ------ | ------ |
| Path | A path to a file. The path separator may be determined by `os.path.sep`. |
| Mode | The desired permission, in octal (e.g. `0700`) |

## makedirs - recursive directory creation


## Create a directory

    os.mkdir('newdir')

If you need to specify permissions, you can use the optional `mode` argument:

    os.mkdir('newdir', mode=0700)

## Get current directory
Use the `os.getcwd()` function:

    print(os.getcwd())

## Determine the name of the operating system
The `os` module provides an interface to determine what type of operating system the code is currently running on.

    os.name

This can return one of the following in Python 3:

 - `posix`
 - `nt`
 - `ce`
 - `java`

More detailed information can be retrieved from [`sys.platform`][1]


  [1]: https://docs.python.org/3/library/sys.html#sys.platform

## Remove a directory
Remove the directory at `path`:

    os.rmdir(path)

You should not use `os.remove()` to remove a directory. That function is for *files* and using it on directories will result in an `OSError`

## Follow a symlink (POSIX)
Sometimes you need to determine the target of a symlink. `os.readlink` will do this:

    print(os.readlink(path_to_symlink))

## Change permissions on a file

    os.chmod(path, mode)

where `mode` is the desired permission, in octal.

