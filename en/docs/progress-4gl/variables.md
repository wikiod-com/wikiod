---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Progress ABL is statically typed. The variables need to be declared and the datatype cannot be changed during run time.

## Syntax
- DEFINE VARIABLE i AS INT64 INITIAL -200 NO-UNDO. //A 64-bit integer initialized to -200

- DEFINE VARIABLE l AS LOGICAL NO-UNDO. //A logical variable named l

- DEFINE VARIABLE c AS CHARACTER NO-UNDO CASE-SENSITIVE. //A case sensitive ('a' <> 'A') variable.

- DEFINE VARIABLE dt AS DATE INTIAL TODAY NO-UNDO. //A date variable set to todays date.

- DEFINE VARIABLE a AS CHARACTER EXTENT 5 NO-UNDO. //An character array with length = 5

- DEFINE VARIABLE j AS INTEGER EXTENT NO-UNDO. //An extent without a set length

- DEFINE VARIABLE b AS DATETIME LABEL "Departure time". //A variable with a label 



## Basic variable declarations
    /*                                                                                                                                                                                                                                                
    These variables are declared with `NO-UNDO`.
    That states that no undo handling is wanted for this specific variable 
    in case of a transactional roll-back. 
    
    This should always be the default unless transactional control over 
    this variable is a requirement. 
    */
    
    /* Strings. A character longer than 32K should be a longchar */
    DEFINE VARIABLE c   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cl  AS LONGCHAR    NO-UNDO.
    
    /* Integers and decimals. INTEGER = 32 bit. INT64 = 64 bits */
    DEFINE VARIABLE i   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE j   AS INT64       NO-UNDO.
    DEFINE VARIABLE k   AS DECIMAL     NO-UNDO.
    
    /* Date and datetimez. Unset variables have the unknown value ? */
    DEFINE VARIABLE d   AS DATE        NO-UNDO.
    DEFINE VARIABLE dt  AS DATETIME    NO-UNDO.
    DEFINE VARIABLE dtz AS DATETIME-TZ NO-UNDO.
    
    /* LOGICAL = Boolean data. True or false (or ?) */
    DEFINE VARIABLE l   AS LOGICAL     NO-UNDO.
    
    /* Rowids and recids are internal identifiers to database records */
    DEFINE VARIABLE rid AS ROWID       NO-UNDO.
    DEFINE VARIABLE rec AS RECID       NO-UNDO.
    
    /* A handle is a handle to anything: a session, an on screen widget etc */
    /* A Com-handle is used for ActiveX Com-automation */
    DEFINE VARIABLE h   AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hc  AS COM-HANDLE  NO-UNDO.
    
    /* A raw variable can contain any data. Binary, strings etc */
    DEFINE VARIABLE rw  AS RAW         NO-UNDO.
    
    /* A mempointer contains a sequence of bytes in memory. */
    DEFINE VARIABLE m   AS MEMPTR      NO-UNDO.



## Arrays - defining and accessing
Progress supports one dimensional arrays, but they are called `EXTENTS`.

    /* Define a character array with the length 5, and display it's length */
    DEFINE VARIABLE a AS CHARACTER EXTENT 5 NO-UNDO.
    DISPLAY EXTENT(a).

Individual positions i the array is accessed using "standard" c-style brackets. But the index starts at 1. The maximum size is 28000.

    a[1] = "A".
    a[2] = "B".
    a[3] = "C".
    a[4] = "D".  
    a[5] = "E".

    DISPLAY a[5].
Result:

[![enter image description here][1]][1]

Index 0 will generate an error:

    DISPLAY a[0].

Result:

[![enter image description here][2]][2]

You can also define a indeterminate array without a set length. The length (extent) can be set in run-time. But only once!

    DEFINE VARIABLE a AS CHARACTER EXTENT NO-UNDO.
    EXTENT(a) = 10.
    EXTENT(a) = 1.

The third line will procude the following error:
[![enter image description here][3]][3]

You can use the `INITIAL` option on the `DEFINE VARIABLE` statement to set initial values. 

    DEFINE VARIABLE a AS CHARACTER EXTENT 3 INITIAL ["one","two","three"] NO-UNDO.
    /* Some statements (like DISPLAY) can handle a whole array: */
    DISPLAY a.

Result:

[![enter image description here][4]][4]

If you don't set all extents the remaining will get the last set value:

    DEFINE VARIABLE a AS CHARACTER EXTENT 10 INITIAL ["one","two","three"] NO-UNDO.
    DISPLAY a.


Result:

[![enter image description here][5]][5]


  [1]: https://i.stack.imgur.com/PsdKC.png
  [2]: https://i.stack.imgur.com/Qlubh.png
  [3]: https://i.stack.imgur.com/YmO90.png
  [4]: https://i.stack.imgur.com/0wV65.png
  [5]: https://i.stack.imgur.com/46oNA.png

## Using the LIKE keyword
Using `LIKE` you can base the definition of you variable on another variable or a field in a database or temp-table.

Defining a variable `LIKE` a database field requiers the database to always be connected. This might not always be what you want.

    DEFINE VARIABLE i AS INTEGER NO-UNDO LABEL "Nr" FORMAT "99999".
    /* Define a variable with the same properties as "i" */
    DEFINE VARIABLE j LIKE i.
    
    /* Define a variable based on Customer.Custnum from the sports2000 database but 
    override the label-definition */    
    DEFINE VARIABLE k LIKE Customer.Custnum LABEL "Client".



