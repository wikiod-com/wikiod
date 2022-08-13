---
title: "Variables and Data Types"
slug: "variables-and-data-types"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Create variable
To create a variable:

    variableType variableName;

For example:

    int a;

To create a variable and initialize it:

    variableType variableName = initialValue;

For example:

    int a = 2;



## Assign value to a variable
If you have a variable declared before, you can assign some value to it:

For example:

    int a;  // declared previously
    a = 2;

Or change the value:

    int a = 3;  // initalized previously
    a = 2;



## Variable types
 - `char` : signed 1-byte character value
 - `byte` : unsigned 8-bit integer
 - `int` : signed 16-bit (on ATMEGA based boards) or 32-bit (on Arduino Due) integer
 - `unsigned int` : unsigned 16-bit (on ATMEGA based boards) or 32-bit (on Arduino Due) integer
 - `long` : signed 32-bit integer
 - `unsigned long` : unsigned 32-bit integer
 - `float` : 4-byte floating point number
 - `double` : 4-byte (on ATMEGA based boards) or 8-byte (on Arduino Due) floating point number

Examples:

    char a = 'A';
    char a = 65;

    byte b = B10010;

    int c = 2;

    unsigned int d = 3;

    long e = 186000L;

    unsigned long f = millis(); // as an example

    float g = 1.117;

    double h = 1.117;








