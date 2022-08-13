---
title: "How does the computational work in cobol?"
slug: "how-does-the-computational-work-in-cobol"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Computational clause is used to describe type of storage used in COBOL. It is used for 3 ways: COMP-1, COMP-2 and COMP-3. The most common form of computational is COMP-3. It frequently is just called "COMP" by programmers.

## COMP-3
Data item is stored in packed decimal format in COMP-3. Packed-decimal format means that each byte of storage (except for the low order byte) can contain two decimal numbers. The low-order byte contains one digit in the leftmost portion and the sign (positive or negative) in the rightmost portion. 

"Zoned decimal format" in the image below is the default storage for a number in COBOL.

[![enter image description here][1]][1]


    01 WS-NUM PIC 9(5) USAGE IS COMP-3 VALUE 21544.

Computational storage is frequently used to reduce the size of a file.


  [1]: https://i.stack.imgur.com/bbLGX.png


## Common implementations
How comp, comp-1 ... comp-5 are implemented is implementation dependent.

    Format     Normal Implementation
    
    Comp       Big endian binary integer
    Comp-1     4 byte floating point       
    Comp-2     8 byte floating point 
    
    Comp-3     Packed decimal 123 is stored as x'123c'
    
    Comp-5     Binary Integer optermised for performance.
               Big Endian on the Mainframe, Little Endian on Intel Hardware
               
Ibm Compilers normally support Comp, Comp-4, Comp-5 in sizes of 2,4,8 bytes. 
GNU Cobolo support sizes of 1,2,4,8.

Comp-1, Comp-2 fields are defined without a picture clause:

    03 Floating-Field      Comp-1.
    03 Double-Field        Comp-2
    
For other Comp's are picture is entered:

    03 Big-Endian           Pic S9(4) Comp.
    03 Packed-Decimal       Pic S9(5) Comp.


