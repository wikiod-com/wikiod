---
title : swig Tutorial
slug : swig-tutorial
weight : 9993
draft : false
images : []
type : docs
---

**SWIG** (Simplified Wrapper and Interface Generator) is a tool for wrapping C and C++ code in a variety of target languages, allowing C/C++ APIs to be used in other languages.

SWIG parses header files and generates code in a manner dependent on the target language. The code generation can be controlled by the developer in the *SWIG interface file* as well as through command line options.

In the interface file, the developer tells SWIG what to wrap and how. SWIG has its own preprocessor system and many special directives to control how data, classes and functions are wrapped in the target language. Some of these directives are general and others are specific to the target language.

Central to how SWIG functions is the *typemap*. Typemaps are rules that specify how types are marshaled between the C code and the target language. Typemaps can be applied globally to everything in the interface file or locally on a case by case basis. They can also be customized if necessary.

Once SWIG is run on the interface file, it produces a C or C++ file which is the wrapper. This file should be compiled and linked with the C/C++ program or static library the wrapper is meant to interface with to produce a shared library. That library in turn is used by the target language.

RTFM
====

It cannot be emphasized enough that SWIG already comes with an [excellent documentation manual][1]. This is very detailed on the one hand, covers [installation][2], and features many concrete examples in the form of code snippets, including a complete "hello world" [SWIG example][3].

But most importantly, it also explains [1.7 How to avoid reading the manual][4]:

> If you hate reading manuals, glance at the "[Introduction][5]" which contains a few simple examples. These examples contain about 95% of everything you need to know to use SWIG. After that, simply use the language-specific chapters as a reference. The SWIG distribution also comes with a large directory of examples that illustrate different topics.


  [1]: http://www.swig.org/Doc3.0/
  [2]: http://www.swig.org/Doc3.0/Preface.html#Preface_installation
  [3]: http://www.swig.org/Doc3.0/Introduction.html#Introduction_nn4
  [4]: http://www.swig.org/Doc3.0/Preface.html#Preface_nn8
  [5]: http://www.swig.org/Doc3.0/Introduction.html#Introduction

