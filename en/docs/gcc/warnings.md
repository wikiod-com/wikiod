---
title: "Warnings"
slug: "warnings"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - **gcc** [**-W***option* [**-W***option* [...]]] *src-file*

## Parameters
| Parameter  | Details |
| ---------- | ------- |
| *option*   | It can be used to enable or disable warnings. It can make warnings into errors. |
| *src-file* | The source file to be compiled. |

It is a good practice to enable most warnings while developing a software.

## Enable nearly all warnings
## C source file ##

    gcc -Wall -Wextra -o main main.c

## C++ source file ##

    g++ -Wall -Wextra -Wconversion -Woverloaded-virtual -o main main.cpp

