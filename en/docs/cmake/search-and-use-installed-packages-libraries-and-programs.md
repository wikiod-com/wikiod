---
title: "Search and use installed packages, libraries and programs"
slug: "search-and-use-installed-packages-libraries-and-programs"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
 - find_package(pkgname [version] [EXACT] [QUIET] [REQUIRED])
 - include(FindPkgConfig)
 - pkg_search_module(prefix [REQUIRED] [QUIET] pkgname [otherpkg...])
 - pkg_check_modules(prefix [REQUIRED] [QUIET] pkgname [otherpkg...])

## Parameters
| **Parameter** | **Details** |
| :-----------: | :---------: |
| version (optional) | Minimum version of the package defined by a major number and optionally a minor, patch and tweak number, in the format major.minor.patch.tweak |
| EXACT (optional) | Specify that the version specified in `version` is the exact version to be found |
| REQUIRED (optional) | Automatically throws an error and stop the process if the package is not found |
| QUIET (optional) | The function won't send any message to the standard output |

- The `find_package` way is compatible on all platform, whereas the `pkg-config` way is available only on Unix-like platforms, like Linux and OSX. 

 - A full description of the `find_package` numerous parameters and options can be found in the [manual](https://cmake.org/cmake/help/v3.6/command/find_package.html).

 - Even though it is possible to specify many optional parameters such as the version of the package, not all Find<package> modules properly uses all those parameters. If any undefined behaviour occur, it could be necessary to find the module in CMake's install path and fix or understand its behaviour.

## Use find_package and Find<package>.cmake modules
The default way to find installed packages with CMake is the use the `find_package` function in conjunction with a `Find<package>.cmake` file. The purpose of the file is to define the search rules for the package and set different variables, such as `<package>_FOUND`, `<package>_INCLUDE_DIRS` and `<package>_LIBRARIES`.

Many `Find<package>.cmake` file are already defined by default in CMake. However, if there is no file for the package you need, you can always write your own and put it inside `${CMAKE_SOURCE_DIR}/cmake/modules` (or any other directory if `CMAKE_MODULE_PATH` was overridden)

A list of default modules can be found in the [manual (v3.6)](https://cmake.org/cmake/help/v3.6/manual/cmake-modules.7.html). It is essential to check the manual according to the version of CMake used in the project or else there could be missing modules. It is also possible to find the installed modules with `cmake --help-module-list`.

There is a nice example for a `FindSDL2.cmake` on [Github](https://github.com/WebAssembly/wasmint/blob/master/cmake/FindSDL2.cmake)

Here's a basic `CMakeLists.txt` that would require SDL2:

    cmake_minimum_required(2.8 FATAL_ERROR)
    project("SDL2Test")
    
    set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules")
    find_package(SDL2 REQUIRED)
    
    include_directories(${SDL2_INCLUDE_DIRS})
    add_executable(${PROJECT_NAME} main.c)
    target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES})
    


## Use pkg_search_module and pkg_check_modules
On Unix-like operating systems, it is possible to use the `pkg-config` program to find and configure packages that provides a `<package>.pc` file.

In order to use `pkg-config`, it is necessary to call `include(FindPkgConfig)` in a `CMakeLists.txt`. Then, there are 2 possible functions:
 - `pkg_search_module`, which checks for the package and uses the first available.
 - `pkg_check_modules`, which check for all the corresponding packages.

Here's a basic `CMakeLists.txt` that uses `pkg-config` to find SDL2 with version above or equal to 2.0.1:

    cmake_minimum_required(2.8 FATAL_ERROR)
    project("SDL2Test")

    include(FindPkgConfig)
    pkg_search_module(SDL2 REQUIRED sdl2>=2.0.1)

    include_directories(${SDL2_INCLUDE_DIRS})
    add_executable(${PROJECT_NAME} main.c)
    target_link_libraries(${PROJECT_NAME} ${SDL2_LIBRARIES})

