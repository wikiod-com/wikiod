---
title: "String"
slug: "string"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## STRINGVAL... Move -versus- STRING
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  STRINGVAL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WORK-AREAS.
           05 I-STRING        PIC X(08) VALUE   'STRNGVAL'.

           05 O-STRING        PIC XBXBXBXBXBXBXBX.
              88  O-STRING-IS-EMPTY     VALUE   SPACES.

       PROCEDURE DIVISION.
       GENESIS.

           PERFORM MAINLINE

           PERFORM FINALIZATION

           GOBACK

            .


       MAINLINE.

           DISPLAY 'STRINGVAL EXAMPLE IS STARTING !!!!!!!!!!!!!!'

           DISPLAY '=== USING MOVE STATEMENT ==='
           MOVE I-STRING TO O-STRING
           DISPLAY 'O STRING= ' O-STRING

           DISPLAY '=== USING STRING STATEMENT ==='
           SET O-STRING-IS-EMPTY    TO  TRUE
           STRING I-STRING ( 1 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 2 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 3 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 4 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 5 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 6 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 7 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               I-STRING ( 8 : 1 ) DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               INTO O-STRING

           DISPLAY 'O STRING= ' O-STRING

            .


       FINALIZATION.

           DISPLAY 'STRINGVAL EXAMPLE IS COMPLETE !!!!!!!!!!!!!!'

            .

       END PROGRAM STRINGVAL.



## Not an example, but ....
seemed the only way to add a comment. 
One thing that's easy to forget is that if you string some variables like the example above, and the resulting length is SHORTER than what was originally in the receiving variable (o- string above) then"trailing"characters are left in place.
 
For example, if o- string contained "the string contains this data" and you string together "fred & Bert", then o- string will contain "fred & Bertontains this data" (if I counted right).

Summa summary, get into the habit of ALWAYS moving spaces into your receiving variable before you start stringing.

