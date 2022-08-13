---
title: "C++"
slug: "c++"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## A Simple Build
It is very easy to build a simple C++ project. Here is an example of a `SConstruct` file that does so:

    env=Environment()

    env.Program('hello', Glob('src/*.cpp'))

This creates the executable `hello` composed of all the sources in `src` with extension `cpp`.

## Specifying Various Build Options
This example shows more detailed build settings:

    env=Environment(
        CPPPATH='/usr/include/boost/',
        CPPDEFINES=['foo'],
        LIBS=['bar'],
        SCONS_CXX_STANDARD='c++11')

    env.Program('hello', Glob('src/*.cpp'))

This builds the executable `hello` from all the `cpp` files in `src`, with the following settings:

* The search path is `/usr/include/boost'

* The constant `FOO` is defined

* The executable links with `bar`

* C++11 is used as a standard


