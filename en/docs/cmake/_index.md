---
title : cmake Tutorial
slug : cmake-tutorial
weight : 9844
draft : false
images : []
type : docs
---

CMake is a tool for defining and managing code builds, primarily for C++.

CMake is a cross-platform tool; the idea is to have a single definition of how the project is built - which translates into specific build definitions for any supported platform.

It accomplishes this by pairing with different platform-specific buildsystems; CMake is an intermediate step, that generates build input for different specific platforms. On Linux, CMake generates Makefiles; on Windows, it can generate Visual Studio projects, and so on.

Build behavior is defined in `CMakeLists.txt` files - one in every directory of the source code. Each directory's `CMakeLists` file defines what the buildsystem should do in that specific directory. It also defines which subdirectories CMake should handle as well.

Typical actions include:

 * Build a library or an executable out of some of the source files in this directory.
 * Add a filepath to the include-path used during build.
 * Define variables that the buildsystem will use in this directory, and in its subdirectories.
 * Generate a file, based on the specific build configuration.
 * Locate a library which is somewhere in the source tree.

The final `CMakeLists` files can be very clear and straightforward, because each is so limited in scope. Each only handles as much of the build as is present in the current directory.

For official resources on CMake, see CMake's [Documentation](https://cmake.org/documentation/) and [Tutorial](https://cmake.org/cmake-tutorial/).

