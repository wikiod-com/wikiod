---
title: "Getting started with octave"
slug: "getting-started-with-octave"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Instructions on getting octave set up or installed.

Installing Octave for debian systems (**Debian**, **Ubuntu**):

Simple: `sudo apt-get install octave`

Advanced:
Well, if you want to install other external packages 

    sudo apt-get install octave-control octave-image octave-io octave-optim octave-signal octave-statistics

For furter details like 
 - install from PPA.
 - install from sources, etc.

A very good detailed wikis are present in Octave's wiki pages

 - For **Debian** or **Ubuntu** look at this - [wiki][3]

 - For **Windows** have a look at this - [wiki][4]

 - And for **Mac OS X** look at this - [wiki][5]

  [3]: http://wiki.octave.org/Octave_for_Debian_systems
  [4]: http://wiki.octave.org/Octave_for_Microsoft_Windows
  [5]: http://wiki.octave.org/Octave_for_MacOS_X

## Hello World
 1. start Octave by running the command `octave` (the executable should be in your path)

 2. type `disp('Hello, World!')` at the Octave command prompt
```
>> disp('Hello, World!')
Hello, World!
```

## Reading commands from a script file
Octave commands can be saved in a file and evaluated by loading the file using `source`.

For instance, let `hello.m` be the text file containing two lines (the first line is a comment)
```
# my first Octave program
disp('Hello, World!')
```

If you type `source hello.m` at an Octave command prompt you will get
```
>> source hello.m
Hello, World!
```

Note that a script file doesn't necessarily have to have the extension `.m`.



## Matrices
Create a 2x3 matrix. Each row is a comma-separated list of elements. Rows are separated by a semicolon.

```
A = [1, 2, 3; 4, 5, 6]

# A =
#
#   1   2   3
#   4   5   6
```

Sum of two matrices
```
B = [1, 1, 1; 1, 1, 1]

# B =
#
#    1   1   1
#    1   1   1

A+B

# ans =
#
#   2   3   4
#   5   6   7
```

Multiply matrix by a scalar
```
2*A

# ans =
#
#    2    4    6
#    8   10   12
```

Matrix multiplication
```
C = [1, 0; 0, 0; 0, 1]

# C =
#
#   1   0
#   0   0
#   0   1

A*C

# ans =
#
#   1   3
#   4   6
```

A matrix can be a column vector
```
C = [2; 0; 1]

# C =
#
#   2
#   0
#   1

A * C
# ans =
# 
#     5
#    14
```

Concatenating matrices

For horizontal concatenation, that is joining two block matrices column-wise 
```
A= [1,2;3,4]; 
B=[4,3;2,1];
C=horzcat(A,B);
disp(C)
# C=
#
# 1 2 4 3 
# 3 4 2 1 






