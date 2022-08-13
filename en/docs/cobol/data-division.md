---
title: "Data division"
slug: "data-division"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

DATA DIVISION is one of the four parts that make up a COBOL program. It contains statements describing the data used by the program. It consists of four sections: FILE SECTION, WORKING-STORAGE SECTION, LOCAL-STORAGE SECTION and LINKAGE SECTION.

## Sections in Data Division
 SECTIONs in COBOL can be required or optional, depending on which DIVISION they are in.

    DATA DIVISION.
    FILE SECTION.
    FD SAMPLE-FILE
    01 FILE-NAME PIC X(20).
    WORKING-STORAGE SECTION.
    01 WS-STUDENT PIC A(10).
    01 WS-ID PIC 9(5).
    LOCAL-STORAGE SECTION.
    01 LS-CLASS PIC 9(3).
    LINKAGE SECTION.
    01 LS-ID PIC 9(5).


In the above example, 01's are level numbers.


----------

Level Number
------------

Level number is used to specify the level of data in a record. They are used to differentiate between elementary items and group items. Elementary items can be grouped together to create group items.


 - 01: Record description entry. Group level number is always 01.


    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 WS-NAME               PIC X(25).   ---> ELEMENTARY ITEM 
    01 WS-SURNAME            PIC X(25).   ---> ELEMENTARY ITEM    
    01 WS-ADDRESS.                        ---> GROUP ITEM   
       05 WS-HOUSE-NUMBER    PIC 9(3).    ---> ELEMENTARY ITEM
       05 WS-STREET          PIC X(15).   ---> ELEMENTARY ITEM 

        


 - 02 to 49: Elementary items
 - 66: Rename Clause items
 - 77: Items which cannot be sub-divided.
 - 88: Level 88 is a special level number used to improve the readability of COBOL programs and to improve IF tests. A level 88 looks like a level under another variable, but it's not. It does not have a PICTURE, but it does have a value. A level 88 is always associated with another variable and is a condition name for that variable. 


     01 YES-NO PIC X.
     88 ANSWER-IS-YES VALUE "Y".

Both of the following conditions test whether YES-NO is equal to "Y":

     IF YES-NO = "Y"
     IF ANSWER-IS-YES

A level 88 condition name can be used for an alphanumeric or numeric variable.


----------


Picture Clause
--------------

The PICTURE CLAUSE defines two things about a variable: the size of the variable (the number of bytes used in memory for the
value) and the type of data that can be stored in the variable.

