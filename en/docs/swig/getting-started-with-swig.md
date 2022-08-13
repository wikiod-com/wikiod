---
title: "Getting started with swig"
slug: "getting-started-with-swig"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
A minimal example of using SWIG.

**HelloWorld.i**, the SWIG interface file

    %module helloworld    //the name of the module SWIG will create
    %{                    //code inside %{...%} gets inserted into the wrapper file
    #include "myheader.h" //helloworld_wrap.cxx includes this header
    %}
    
    %include "myheader.h"   //include the header for SWIG to parse

Then, in the command line

    swig -c++ -java HelloWorld.i
which means we are wrapping C++ (as opposed to C) with Java as the target language as specified by HelloWorld.i. This will produce a C++ file, helloworld_wrap.cxx, which has the wrapper code. This file should be compiled and linked against whatever code the wrapper is supposed to interface with (e.g., a static library) to produce a shared library.
With some languages, as with Java in our example, additional code will be generated - in our case, there will be at least one Java class file.

## Installation or Setup
Detailed instructions on getting swig set up or installed.

