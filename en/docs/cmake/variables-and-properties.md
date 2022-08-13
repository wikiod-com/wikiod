---
title: "Variables and Properties"
slug: "variables-and-properties"
draft: false
images: []
weight: 9888
type: docs
toc: true
---

The simplicity of basic CMake variables belies the complexity of the full variable syntax. This page documents the various variable cases, with examples, and points out the pitfalls to avoid.

## Syntax
- set(variable_name value [CACHE type description [FORCE]])


Variable names are case-sensitive. Their values are of type string. The value of a variable is referenced via:

    ${variable_name}

and is evaluated inside a quoted argument

    "${variable_name}/directory"

## Variables and the Global Variables Cache
Mostly you will use ["normal variables"](https://cmake.org/cmake/help/latest/command/set.html#set-normal-variable):

    set(VAR TRUE)
    set(VAR "main.cpp")
    set(VAR1 ${VAR2})

But CMake does also know global ["cached variables"](https://cmake.org/cmake/help/latest/command/set.html#set-cache-entry) (persisted in `CMakeCache.txt`). And if normal and cached variables of the same name exist in the current scope, normal variables do hide the cached ones:

    cmake_minimum_required(VERSION 2.4)
    project(VariablesTest)

    set(VAR "CACHED-init" CACHE STRING "A test")
    message("VAR = ${VAR}")

    set(VAR "NORMAL")
    message("VAR = ${VAR}")

    set(VAR "CACHED" CACHE STRING "A test" FORCE)
    message("VAR = ${VAR}")

**First Run's Output**

    VAR = CACHED-init
    VAR = NORMAL
    VAR = CACHED

**Second Run's Output**

    VAR = CACHED
    VAR = NORMAL
    VAR = CACHED

*Note:* The `FORCE` option does also unset/remove the normal variable from the current scope.

# Use Cases for Cached Variables #

There are typically two use cases (please don't misuse them for global variables):

1. An value in your code should be modifiable from your project's user e.g. with the [`cmakegui`](https://cmake.org/cmake/help/latest/manual/cmake-gui.1.html), [`ccmake`](https://cmake.org/cmake/help/latest/manual/ccmake.1.html) or with [`cmake -D ...`](https://cmake.org/cmake/help/latest/manual/cmake.1.html#options) option:

   **CMakeLists.txt** / **MyToolchain.cmake**

       set(LIB_A_PATH "/some/default/path" CACHE PATH "Path to lib A")

   **Command Line**

       $ cmake -D LIB_A_PATH:PATH="/some/other/path" ..

   This does pre-set this value in the cache and the above line will not modify it.

   **CMake GUI**

   [![enter image description here][1]][1]       

   In the GUI the user first starts the configuration process, then can modify any cached value and finishes with starting the build environment generation.


2. Additionally CMake does cache search/test/compiler identification results (so it does not need to do it again whenever re-running the configuration/generation steps)

       find_path(LIB_A_PATH libA.a PATHS "/some/default/path")

   Here `LIB_A_PATH` is created as a cached variable. 


  [1]: http://i.stack.imgur.com/YFDoN.png

## Strings and Lists
It's important to know how CMake distinguishes between lists and plain strings. When you write:

`set(VAR "a b c")`

you create a **string** with the value `"a b c"`. But when you write this line without quotes:

`set(VAR a b c)`

You create a **list** of three items instead: `"a"`, `"b"` and `"c"`.

Non-list variables are actually lists too (of a single element).

Lists can be operated on with the `list()` command, which allows concatenating lists, searching them, accessing arbitrary elements and so on ([documentation of list()](https://cmake.org/cmake/help/latest/command/list.html)).

Somewhat confusing, a **list** is also a **string**. The line

`set(VAR a b c)`

is equivalent to

`set(VAR "a;b;c")`

Therefore, to concatenate lists one can also use the `set()` command:

`set(NEW_LIST "${OLD_LIST1};${OLD_LIST2})"`

## Cached (Global) Variable
```
set(my_global_string "a string value"
    CACHE STRING "a description about the string variable")
set(my_global_bool TRUE
    CACHE BOOL "a description on the boolean cache entry")
```

In case a cached variable is already defined in the cache when CMake processes the respective line (e.g. when CMake is rerun), it is not altered.
To overwrite the default, append `FORCE` as the last argument:
```
set(my_global_overwritten_string "foo"
    CACHE STRING "this is overwritten each time CMake is run" FORCE)
```

## Adding profiling flags to CMake to use gprof
The series of events here is supposed to work as follows:

 1. Compile code with -pg option
 2. Link code with -pg option
 3. Run program
 4. Program generates gmon.out file
 5. Run gprof program

To add profiling flags, you must add to your CMakeLists.txt:

    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg")
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pg")
    SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -pg")

That must add flags to compile and link, and use after execute the program:

gprof ./my_exe

If you get an error like:

    gmon.out: No such file or directory

That means that compilation didn't add profiling info properly.


## Local Variable
```
set(my_variable "the value is a string")
```

By default, a local variable is only defined in the current directory and any subdirectories added through the `add_subdirectory` command.

To extend the scope of a variable there are two possibilities:

1. `CACHE` it, which will make it globally available
1. use `PARENT_SCOPE`, which will make it available in the parent scope. The parent scope is either the `CMakeLists.txt` file in the parent directory or caller of the current function. 

    Technically the parent directory will be the `CMakeLists.txt` file that included the current file via the `add_subdirectory` command.


