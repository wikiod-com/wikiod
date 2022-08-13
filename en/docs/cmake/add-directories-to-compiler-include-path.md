---
title: "Add Directories to Compiler Include Path"
slug: "add-directories-to-compiler-include-path"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- include_directories([AFTER|BEFORE] [SYSTEM] dir1 [dir2 ...])

## Parameters
| Parameter | Description |
|-----------|-------------|
| `dirN` | one ore more relative or absolute paths |
| `AFTER`, `BEFORE` | (optional) whether to add the given directories to the front or end of the current list of include paths; default behaviour is defined by `CMAKE_INCLUDE_DIRECTORIES_BEFORE` |
| `SYSTEM` | (optional) tells the compiler to tread the given directories as _system include dirs_, which might trigger special handling by the compiler |

## Add a Project's Subdirectory
Given the following project structure
```
include\
  myHeader.h
src\
  main.cpp
CMakeLists.txt
```
the following line in the `CMakeLists.txt` file
```
include_directories(${PROJECT_SOURCE_DIR}/include)
```
adds the `include` directory to the _include search path_ of the compiler for all targets defined in this directory (and all its subdirectories included via [`add_subdirectory()`](https://www.wikiod.com/cmake/hierarchical-project#Simple approach without packages)).

Thus, the file `myHeader.h` in the project's `include` subdirectory can be included via `#include "myHeader.h"` in the `main.cpp` file.

