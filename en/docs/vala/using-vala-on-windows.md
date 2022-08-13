---
title: "Using Vala on Windows"
slug: "using-vala-on-windows"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This topic focuses on how to get valac running on Windows.

## Using msys2 (64 Bit)
1. Install msys2 (http://www.msys2.org/)

2. Install the required prerequisites for Vala

       pacman -S mingw64/mingw-w64-x86_64-gcc
       pacman -S mingw64/mingw-w64-x86_64-pkg-config
       pacman -S mingw64/mingw-w64-x86_64-vala

   and all the additional packages your code requires, i.e.

       pacman -S mingw64/mingw-w64-x86_64-libgee
       ...

3. Launch the correct msys2 shell

       C:\msys64\mingw64.exe

4. Check the `MSYSTEM` and `PKG_CONFIG_PATH` environment variables

       $ echo $MSYSTEM
       MINGW64
    
       $ echo $PKG_CONFIG_PATH
       /mingw64/lib/pkgconfig:/mingw64/share/pkgconfig

5. Run `valac` as usual, but make sure to always work in the correct environment (see steps 3 and 4)

    For example let's build the [first GeeSample][1] here:

       $ valac gee-list.vala --pkg gee-0.8


  [1]: https://wiki.gnome.org/Projects/Vala/GeeSamples?highlight=%28%5CbVala%2FExamples%5Cb%29

