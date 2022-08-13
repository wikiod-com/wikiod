---
title: "MOVE statement"
slug: "move-statement"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

`MOVE` is the workhorse of COBOL.  Data is moved from literal or identifier to one or more identifiers.  COBOL has a distinction between *elementary* and *group* MOVE.  Elementary data is type converted from source to destination. Group data is moved as a byte array, without regard to field types with a structure.  Numeric fields are moved from right to left, high order digit truncation with zero fill (normally).  Alphanumeric character data is moved left to right, right end character truncation with space fill.  There are quite a few rules on how `MOVE` goes about its business, with both BINARY and PICTURE DISPLAY data forms, and group hierarchies all accounted for.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/Ko0pL.png

## Some MOVE details, there are many
    01 a PIC 9.
    01 b PIC 99.
    01 c PIC 999.

    01 s PIC X(4).

    01 record-group.
       05 field-a PIC 9.
       05 field-b PIC 99.
       05 field-c PIC 999.
    01 display-record.
       05 field-a PIC Z.
       05 field-b PIC ZZ.
       05 field-c PIC $Z9.

    *> numeric fields are moved left to right
    *> a set to 3, b set to 23, c set to 123
    MOVE 123 TO a b c

    *> moves can also be by matching names within groups
    MOVE a TO field-a OF record-group
    MOVE b TO field-b OF record-group
    MOVE c TO field-c OF record-group
    MOVE CORRESPONDING record-group TO display-record

    *> character data is moved right to left
    *> s will be set to xyzz
    MOVE "xyzzy" TO s

