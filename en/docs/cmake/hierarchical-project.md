---
title: "Hierarchical project"
slug: "hierarchical-project"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Simple approach without packages
Example that builds an executable (editor) and links a library (highlight) to it. Project structure is straightforward, it needs a master CMakeLists and a directory for each subproject:

    CMakeLists.txt
    editor/
        CMakeLists.txt
        src/
            editor.cpp
    highlight/
        CMakeLists.txt
        include/
            highlight.h
        src/
            highlight.cpp

The master CMakeLists.txt contains global definitions and `add_subdirectory` call for each subproject:

    cmake_minimum_required(VERSION 3.0)
    project(Example)

    add_subdirectory(highlight)
    add_subdirectory(editor)

CMakeLists.txt for the library assigns sources and include directories to it. By using `target_include_directories()` instead of `include_directories()` the include dirs will be propagated to library users:

    cmake_minimum_required(VERSION 3.0)
    project(highlight)

    add_library(${PROJECT_NAME} src/highlight.cpp)
    target_include_directories(${PROJECT_NAME} PUBLIC include)

CMakeLists.txt for the application assigns sources and links the highlight library. Paths to hightlighter's binary and includes are handled automaticaly by cmake:

    cmake_minimum_required(VERSION 3.0)
    project(editor)

    add_executable(${PROJECT_NAME} src/editor.cpp)
    target_link_libraries(${PROJECT_NAME} PUBLIC highlight)

