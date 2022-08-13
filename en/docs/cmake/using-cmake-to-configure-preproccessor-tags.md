---
title: "Using CMake to configure preproccessor tags"
slug: "using-cmake-to-configure-preproccessor-tags"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The use of CMake in a C++ project if used correctly can allow the programmer to focus less on the platform, program version number and more on the actual program itself. With CMake you can define preprocessor tags that allow for easy checking of which platform or any other preprocessor tags you might need in the actual program. Such as the version number which could be leveraged in a log system.

## Syntax
-    #define preprocessor_name "@cmake_value@"

It is important to understand not every preprocessor should be defined in the `config.h.in`. Preprocessor tags are generally used only to make the programmers life easier and should be used with discretion. You should research if a preprocessor tag already exists before defining it as you may run into undefined behavior on different system.

## Using CMake to define the version number for C++ usage
The possibilities are endless. as you can use this concept to pull the version number from your build system; such as git and use that version number in your project.

**CMakeLists.txt**

    cmake_minimum_required(VERSION 3.8)
    project(project_name VERSION "0.0.0")
    
    configure_file(${path to configure file 'config.h.in'}
    include_directories(${PROJECT_BINARY_BIN}) // this allows the 'config.h' file to be used throughout the program

    ...

**config.h.in**

    #ifndef INCLUDE_GUARD
    #define INCLUDE_GUARD
    
    #define PROJECT_NAME "@PROJECT_NAME@"
    #define PROJECT_VER  "@PROJECT_VERSION@"
    #define PROJECT_VER_MAJOR "@PROJECT_VERSION_MAJOR@"
    #define PROJECT_VER_MINOR "@PROJECT_VERSION_MINOR@"
    #define PTOJECT_VER_PATCH "@PROJECT_VERSION_PATCH@"
    
    #endif // INCLUDE_GUARD

**main.cpp**

    #include <iostream>
    #include "config.h"
    int main()
    {
        std::cout << "project name: " << PROJECT_NAME << " version: " << PROJECT_VER << std::endl;
        return 0;
    }


**output**

    project name: project_name version: 0.0.0



