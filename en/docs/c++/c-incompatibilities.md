---
title: "C incompatibilities"
slug: "c-incompatibilities"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This describes what C code will break in a C++ compiler.

## Reserved Keywords
The first example are keywords that have a special purpose in C++: the following is legal in C, but not C++.

    int class = 5

These errors are easy to fix: just rename the variable.


## Weakly typed pointers
In C, pointers can be cast to a `void*`, which needs an explicit cast in C++. The following is illegal in C++, but legal in C:

    void* ptr;
    int* intptr = ptr;
Adding an explicit cast makes this work, but can cause further issues.

## goto or switch
In C++, you may not skip initializations with `goto` or `switch`. The following is valid in C, but not C++:

    goto foo;
    int skipped = 1;
    foo;
These bugs may require redesign. 



