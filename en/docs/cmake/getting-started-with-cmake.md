---
title: "Getting started with cmake"
slug: "getting-started-with-cmake"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## CMake Installation
Head over to [CMake][1] download page and get a binary for your operating system, e.g. Windows, Linux, or Mac OS X. On Windows double click the binary to install. On Linux run the binary from a terminal.

On Linux, you can also install the packages from the distribution's package manager. 
On Ubuntu 16.04 you can install the command-line and graphical application with:

    sudo apt-get install cmake
    sudo apt-get install cmake-gui

On FreeBSD you can install the command-line and the Qt-based graphical application with:

    pkg install cmake
    pkg install cmake-gui

On Mac OSX, if you use one of the package managers available to install your software, the most notable being MacPorts ([MacPorts][2]) and Homebrew ([Homebrew][3]), you could also install CMake via one of them. For example, in case of MacPorts, typing the following  

    sudo port install cmake  
will install CMake, while in case you use the Homebrew package manger you will type

    brew install cmake

Once you have installed CMake you can check easily by doing the following

    cmake --version

You should see something similar to the following

    cmake version 3.5.1
    
    CMake suite maintained and supported by Kitware (kitware.com/cmake).


  [1]: https://cmake.org/download/
  [2]: https://www.macports.org/
  [3]: http://brew.sh/

## Simple "Hello World" Project
Given a C++ source file `main.cpp` defining a `main()` function, an accompanying `CMakeLists.txt` file (with the following content) will instruct _CMake_ to generate the appropriate build instructions for the current system and default C++ compiler.

**main.cpp** *([C++ Hello World Example](https://www.wikiod.com/docs/c%2b%2b/206/hello-world))*

    #include <iostream>

    int main()
    {
        std::cout << "Hello World!\n";
        return 0;
    }

**CMakeLists.txt**

    cmake_minimum_required(VERSION 2.4)

    project(hello_world)

    add_executable(app main.cpp)

[<kbd>See it live on Coliru</kbd>](http://coliru.stacked-crooked.com/a/cf4bdd447ec1e743)

1. [`cmake_minimum_required(VERSION 2.4)`](https://cmake.org/cmake/help/latest/command/cmake_minimum_required.html) sets a minimum CMake version required to evaluate the current script.

1. [`project(hello_world)`](https://cmake.org/cmake/help/latest/command/project.html) starts a new CMake project.
This will trigger a lot of internal CMake logic, especially the detection of the default C and C++ compiler.

1. With [`add_executable(app main.cpp)`](https://cmake.org/cmake/help/latest/command/add_executable.html) a build target `app` is created, which will invoke the configured compiler with some default flags for the current setting to compile an executable `app` from the given source file `main.cpp`.

**Command Line** *(In-Source-Build, not recommended)*

    > cmake .
    ...
    > cmake --build .

[`cmake .`](https://cmake.org/cmake/help/latest/manual/cmake.1.html) does the compiler detection, evaluates the `CMakeLists.txt` in the given `.` directory and generates the build environment in the current working directory. 

The `cmake --build .` command is an abstraction for the necessary build/make call.

**Command Line** *(Out-of-Source, recommended)*

To keep your source code clean from any build artifacts you should do "out-of-source" builds.

    > mkdir build
    > cd build
    > cmake ..
    > cmake --build .

Or CMake can also abstract your platforms shell's basic commands from above's example:

    > cmake -E make_directory build
    > cmake -E chdir build cmake .. 
    > cmake --build build 



## "Hello World" as a library
This example shows how to deploy the "Hello World" program as a library and how to link it with other targets.

Say we have the same set of source/header files as in the *https://www.wikiod.com/docs/cmake/862/getting-started-with-cmake/22391/hello-world-with-multiple-source-files#t=201610310659039267444* example. Instead of building from multiple source files, we can first deploy `foo.cpp` as a library by using [`add_library()`][1] and afterwards linking it with the main program with [`target_link_libraries()`][2].

We modify **CMakeLists.txt** to

    cmake_minimum_required(VERSION 2.4)
    
    project(hello_world)
    
    include_directories(${PROJECT_SOURCE_DIR})
    add_library(applib foo.cpp)
    add_executable(app main.cpp)
    target_link_libraries(app applib)

and following the same steps, we'll get the same result.


  [1]: https://cmake.org/cmake/help/v3.0/command/add_library.html
  [2]: https://cmake.org/cmake/help/v3.0/command/target_link_libraries.html

## Switching between build types, e.g. debug and release
CMake knows several build types, which usually influence default compiler and linker parameters (such as debugging information being created) or alternative code paths.

By default, CMake is able to handle the following build types:

* **Debug**: Usually a classic debug build including debugging information, no optimization etc.
* **Release**: Your typical release build with no debugging information and full optimization.
* **RelWithDebInfo:**: Same as *Release*, but with debugging information.
* **MinSizeRel**: A special *Release* build optimized for size.

How configurations are handled depends on the generator that is being used.

Some generators (like Visual Studio) support multiple configurations. CMake will generate all configurations at once and you can select from the IDE or using `--config CONFIG` (with `cmake --build`) which configuration you want to build. For these generators CMake will try its best to generate a build directory structure such that files from different configurations do not step on each other.

Generators that do only support a single configuration (like Unix Makefiles) work differently. Here the currently active configuration is determined by the value of the CMake variable `CMAKE_BUILD_TYPE`.

For example, to pick a different build type one could issue the following command line commands:

    cmake -DCMAKE_BUILD_TYPE=Debug path/to/source
    cmake -DCMAKE_BUILD_TYPE=Release path/to/source

A CMake script should avoid setting the `CMAKE_BUILD_TYPE` itself, as it's generally considered the users responsibility to do so.

For single-config generators switching the configuration requires re-running CMake. A subsequent build is likely to overwrite object files produced by the earlier configuration.

## "Hello World" with multiple source files
First we can specify the directories of header files by [`include_directories()`][1], then we need to specify the corresponding source files of the target executable by [`add_executable()`][2], and be sure there's exactly one `main()` function in the source files.

Following is a simple example, all the files are assumed placed in the directory `PROJECT_SOURCE_DIR`.

**main.cpp**

    #include "foo.h"
    
    int main()
    {
        foo();
        return 0;
    }

**foo.h**

    void foo();

**foo.cpp**

    #include <iostream>
    #include "foo.h"
    
    void foo()
    {
        std::cout << "Hello World!\n";
    }

**CMakeLists.txt**

    cmake_minimum_required(VERSION 2.4)
    
    project(hello_world)
    
    include_directories(${PROJECT_SOURCE_DIR})
    add_executable(app main.cpp foo.cpp)  # be sure there's exactly one main() function in the source files


We can follow the same procedure in the [above example][3] to build our project. Then executing `app` will print

    >./app
    Hello World!


  [1]: https://cmake.org/cmake/help/v3.0/command/include_directories.html
  [2]: https://cmake.org/cmake/help/v3.0/command/add_executable.html
  [3]: https://www.wikiod.com/docs/cmake/862/introduction-to-cmake/7501/simple-hello-world-project

