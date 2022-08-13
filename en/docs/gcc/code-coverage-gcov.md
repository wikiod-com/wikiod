---
title: "Code coverage gcov"
slug: "code-coverage-gcov"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

GCC provide some documentation of gcov [here](https://gcc.gnu.org/onlinedocs/gcc/Gcov.html)

[Gcovr](http://gcovr.com/) and [Lcov](http://ltp.sourceforge.net/coverage/lcov.php) can be used to help generate and summarize the coverage results

## Introduction
Code coverage is a measure used to how often each source code statement and branch is executed. This measure is usually required when running a test suite to ensure that as much of the code as possible is tested by the test suite. It can also be used during profiling to determine code hot-spots and thus where optimization efforts may have the most effect.

In GCC code coverage is provided by the gcov utility. gcov works only with code compiled with gcc with particular flags. There are very few other compilers with which gcov works at all.

## Compilation
Before using gcov, source code should be compiled with gcc using the two flags, `-fprofile-arcs` and  `-ftest-coverage`. This tells the compiler to generate the information and extra object file code required by gcov.

    gcc -fprofile-arcs -ftest-coverage hello.c

Linking should also use the `-fprofile-arcs` flag.

## Generate Output
To generate the coverage information the compiled program should be executed. When creating code coverage for a test suite this execution step will normally be performed by the test suite so that the coverage shows what parts of the program the tests executes and which they do not.

    $ a.out

Executing the program will cause a `.gcda` file to be generated in the same directory as the object file.

Subsequently you can call gcov with the program's source file name as an argument to produce a listing of the code with frequency of execution for each line.

    $ gcov hello.c
    File 'hello.c'
    Lines executed:90.00% of 10
    Creating 'hello.c.gcov'

The result is contained in a `.gcov` file. Here is a sample:

             -:    0:Source:hello.c
             -:    0:Graph:hello.gcno
             -:    0:Data:hello.gcda
             -:    0:Runs:1
             -:    0:Programs:1
             -:    1:#include <stdio.h>
             -:    2:
             -:    3:int main (void)
             1:    4:{
             1:    5:  int i;
             -:    6:
             1:    7:  i = 0;
             -:    8:
             -:    9:
             1:   10:  if (i != 0)
         #####:   11:    printf ("Goodbye!\n");
             -:   12:  else
             1:   13:    printf ("Hello\n");
             1:   14:  return 0;
             -:   15:}

Here you can see the line numbers and source and the number of times each line executed. If a line did not execute it is marked with `#####`.

The execution counts are cumulative. If the example program were executed again without removing the .gcda file, the count for the number of times each line in the source was executed would be added to the results of the previous run.

