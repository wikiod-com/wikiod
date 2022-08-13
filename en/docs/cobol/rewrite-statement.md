---
title: "REWRITE statement"
slug: "rewrite-statement"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

The REWRITE statement logically replaces existing records on mass storage.

[![REWRITE statement syntax diagram][1]][1]


  [1]: https://i.stack.imgur.com/HytlW.png

## REWRITE of records in a RELATIVE access file
    GCobol >>SOURCE FORMAT IS FIXED
          *> ***************************************************************
          *> Purpose:   RELATIVE file organization  REWRITE example
          *> Tectonics: cobc -g -debug -W -x relatives.cob
          *> ***************************************************************
           identification division.
           program-id. relatives.
    
           environment division.
           configuration section.
           repository.
               function all intrinsic.
    
           input-output section.
           file-control.
               select optional relatives
                   assign to "relatives.dat"
                   file status is filestatus
                   organization is relative
                   access mode is dynamic
                   relative key is nicknum.
    
           data division.
           file section.
           fd relatives.
              01 person.
                 05 firstname      pic x(48).
                 05 lastname       pic x(64).
                 05 relationship   pic x(32).
    
           working-storage section.
           77 filestatus pic 9(2).
              88 ineof value 1 when set to false is 0.
    
           77 satisfaction pic 9.
              88 satisfied value 1 when set to false is 0.
    
           77 nicknum   pic 9(2).
    
           77 title-line pic x(34).
              88 writing-names value "Adding, Overwriting.  00 to finish".
              88 reading-names value "Which record?         00 to quit".
           77 problem   pic x(80).
    
           screen section.
           01 detail-screen.
              05           line 1 column 1  from title-line erase eos.
              05           line 2 column 1  value "Record: ".
              05 pic 9(2)  line 2 column 16 using nicknum.
              05           line 3 column 1  value "First name: ".
              05 pic x(48) line 3 column 16 using firstname.
              05           line 4 column 1  value "Last name: ".
              05 pic x(64) line 4 column 16 using lastname.
              05           line 5 column 1  value "Relation: ".
              05 pic x(32) line 5 column 16 using relationship.
              05 pic x(80) line 6 column 1  from problem.
    
           01 show-screen.
              05           line 1 column 1  from title-line erase eos.
              05           line 2 column 1  value "Record: ".
              05 pic 9(2)  line 2 column 16 using nicknum.
              05           line 3 column 1  value "First name: ".
              05 pic x(48) line 3 column 16 from firstname.
              05           line 4 column 1  value "Last name: ".
              05 pic x(64) line 4 column 16 from lastname.
              05           line 5 column 1  value "Relation: ".
              05 pic x(32) line 5 column 16 from relationship.
              05 pic x(80) line 6 column 1  from problem.
    
          *> -*********-*********-*********-*********-*********-*********-**
           procedure division.
           beginning.
    
          *> Open the file and find the highest record number
          *> which is a sequential read operation after START
               open input relatives
    
               move 99 to nicknum
               start relatives key is less than or equal to nicknum
                   invalid key
                       move concatenate('NO START' space filestatus)
                         to problem
                       move 00 to nicknum
                   not invalid key
                       read relatives next end-read
               end-start
    
          *> Close and open for i-o
               close relatives
               open i-o relatives
    
          *> Prompt for numbers and names to add until 00
               set writing-names to true
               set satisfied to false
               perform fill-file through fill-file-end
                   until satisfied
    
               close relatives
    
          *> Prompt for numbers to view names of until 00
               open input relatives
    
               set reading-names to true
               set satisfied to false
               perform record-request through record-request-end
                   until satisfied
    
               perform close-shop
           .
           ending.
               goback.

          *> get some user data to add
           fill-file.
               display detail-screen.
               accept detail-screen.
               move spaces to problem
               if nicknum equal 0
                   set satisfied to true
                   go to fill-file-end
               end-if.
           .
           write-file.
               write person
                   invalid key
                       move concatenate("overwriting: " nicknum) to problem
                       REWRITE person
                           invalid key
                               move concatenate(
                                   exception-location() space nicknum
                                   space filestatus)
                               to problem
                       END-REWRITE
               end-write.
               display detail-screen

           .
           fill-file-end.
           .

          *> get keys to display
           record-request.
               display show-screen
               accept show-screen
               move spaces to problem
               if nicknum equals 0
                   set satisfied to true
                   go to record-request-end
               end-if
           .

          *> The magic of relative record number reads
           read-relation.
               read relatives
                   invalid key
                       move exception-location() to problem
                   not invalid key
                       move spaces to problem
               end-read
               display show-screen
           .

           record-request-end.
           .

          *> get out <*
           close-shop.
               close relatives.
               goback.
           .
           end program relatives.


