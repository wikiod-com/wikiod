---
title: "Introduction to Typemaps"
slug: "introduction-to-typemaps"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Typemaps are the very heart of what SWIG does. When you want to pass data between  languages the behaviours for doing so depend upon the type that SWIG sees. The power of typemaps is that the chunks of code are applied many times.

SWIG itself includes many useful typemaps in the core library it is supplied with, e.g. for primitive types, C++ standard library containers, boost etc. so often you won't even need to write any typemaps to expose your code, however that list is by no means complete.

## Syntax
- %typemap(NAME) TYPENAME %{ CODE %}
- %typemap(NAME,OPTION=VALUE) TYPENAME %{ CODE %}
- %typemap(NAME) TYPENAME VARIABLENAME %{ CODE %}
- %typemap(NAME) TYPENAME (LOCALVARTYPE LOCALVARNAME) %{ CODE %}

## Parameters
| Parameter | Details |
| ------ | ------ |
| NAME   | The name of the typemap defines its role in generating a module. `in` and `out` are common and used for input (to C++ or C function calls from Python/Java etc.) and output (i.e. return values from C or C++ to Pyton/Java)   |
| TYPENAME | Each typemap gets applied to one or more matching types. These need to be listed here. Type qualifiers (e.g. const) matters. |
| CODE | Every typemap needs to convert between the C or C++ type and the corresponding type in the wrapped language. You will need to write code to do that in your custom typemaps, typically making use of special variables that get substituted in. E.g. `$input` inside an `in` typemap represents the value from Python/Java, `$result` in an `out` typemap is the thing being returned to Python/Java. `$1` in both in/out typemaps represents the C or C++ variable of the type used to match the typemap. So for `in` typemaps you would assign to `$1` and for `out` you would read from it. (See multi-argument typemaps for more details on why it is a number) |
| VARIABLENAME | Typemaps are matched from most specific to least specific in general.  You can define a typemap that will only match function arguments with specific names by using this optional form. (See typemap matching for more details) |
| (LOCALVARTYPE LOCALVARNAME) | Sometimes, particuarly for `in` typemaps it's useful to be able to declare extra local variables to hold objects around a call. This optional syntax allows us do do that. By using this syntax instead of writing it in `CODE` the scope is altered, but more importantly the variable can be automatically renamed by SWIG to avoid clashes if a function has two arguments using the same typemap.  |
| OPTION=VALUE | The behaviour of some typemaps can be influenced by setting extra options using this syntax. For example a `in` typemap can be made to take no input from the target language by setting `numinputs=0`, in which case the typemap is expected to fill the input implicitly. (A common case for this might be to set something to `NULL`, or fill it from a global value)

## Basic typemap - Python
Given the following custom Boolean type we want to wrap:
<!-- language: c -->

    typedef char MYBOOL;
    #define TRUE 1
    #define FALSE 0

A simple approach might be to write the following typemaps in our SWIG interface:

    %typemap(in) MYBOOL %{
      // $input is what we got passed from Python for this function argument
      $1 = PyObject_IsTrue($input);
      // $1 is what will be used for the C or C++ call and we are responsible for setting it
    %}
    
    %typemap(out) MYBOOL %{
      // $1 is what we got from our C or C++ call
      $result = PyBool_FromLong($1);
      // $result is what gets given back to Python and we are responsible for setting it
    %}

With these typemaps, SWIG will insert our code into the generated wrapper every time it sees a MYBOOL passed into or out of a function call.

