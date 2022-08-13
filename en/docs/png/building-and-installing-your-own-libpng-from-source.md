---
title: "Building and installing your own libpng from source"
slug: "building-and-installing-your-own-libpng-from-source"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Simple "cmake" build and install
If you want to use "cmake" (see www.cmake.org), type

    cd [your libpng source directory]
    cmake . -DCMAKE_INSTALL_PREFIX=/path
    make
    make install

where "/path" points to the installation directory where you want to put the libpng "lib", "include", and "bin" subdirectories.


## Building and installing libpng with "git" and "configure"
This will download libpng from the official "git" repository and build it in your "libpng" directory.

    git clone https://github.com/glennrp/libpng.git libpng
    cd libpng
    ./autogen.sh
    ./configure [--prefix=/path]
    make install

where "/path" points to the installation directory where you want to put the libpng "lib", "include", and "bin" subdirectories.

