---
title : cython Tutorial
slug : cython-tutorial
weight : 9947
draft : false
images : []
type : docs
---

## What is Cython? ##
The Cython programming language enriches Python by C-like static typing, the ability to directly call C functions, and several other features. This allows to reach C-level performance while still using a Python-like syntax.

## How does it work? ##
Cython code is compiled using the cython source-to-source compiler to create C or C++ code, which in turn can be compiled using a C compiler. This allows to create extensions that can be imported from Python or executables.

The main performance gain Cython can reach in contrast to pure Python stems from bypassing the CPython API. For example when adding two integers, Python performs a type check for each variable, finds an add function that satisfies the found types, and calls that function. In the Cython-generated C code, the types are already know and only one function call to is made.
Hence, Cython especially shines for mathematic problems in which the types are clear.

## How do I use it to speed up my code?  ##
A common use case, when trying to speed up a program using Cython, is to profile the code and move the computationally expensive parts to compiled Cython modules. This allows to retain Python syntax for the bulk of the code and apply the speedup where it is most needed.

