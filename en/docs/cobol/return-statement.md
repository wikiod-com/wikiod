---
title: "RETURN statement"
slug: "return-statement"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

The `RETURN` statement controls when data is sent to the internal COBOL sort algorithm writer, as part of an `OUTPUT PROCEDURE`.  Post sort data can be transformed under programmer control before being returned and written to the output file by the sort algorithm.

[![RETURN statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/3FCtz.png

## RETURN a record to SORT OUTPUT PROCEDURE
This is a seedwork sample.  The `SORT` `OUTPUT PROCEDURE` could manipulate the sorted records before they are returned to the write portion of the internal COBOL sort algorithm.  In this case, no transformation is done, `work-rec` is directly moved to `out-rec`.

    GCobol >>SOURCE FORMAT IS FIXED
          ******************************************************************
          * Purpose:   A GnuCOBOL SORT verb example
          * Tectonics: cobc -x sorting.cob
          *     ./sorting <input >output
          *   or simply
          *     ./sorting
          *   for keyboard and screen demos
          ******************************************************************
           identification division.
           program-id. sorting.
    
           environment division.
           configuration section.
          * Set up a sort order where lower and upper case stay together
           special-names.
               alphabet mixed is " aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTu
          -"UvVwWxXyYzZ0123456789".
    
           input-output section.
           file-control.
               select sort-in
                   assign keyboard
                   organization is line sequential.
               select sort-out
                   assign display
                   organization is line sequential.
               select sort-work
                   assign "sortwork".
    
           data division.
           file section.
           fd sort-in.
              01 in-rec        pic x(255).
           fd sort-out.
              01 out-rec       pic x(255).
           sd sort-work.
              01 work-rec      pic x(255).
    
           working-storage section.
           01 loop-flag        pic x value low-value.
    
           procedure division.
           sort sort-work
               on descending key work-rec
               collating sequence is mixed
               input procedure is sort-reader
               output procedure is sort-writer.
    
           display sort-return.
           goback.
    
          ******************************************************************
           sort-reader.
           move low-value to loop-flag
           open input sort-in
           read sort-in
               at end move high-value to loop-flag
           end-read
           perform
               until loop-flag = high-value
                   move in-rec to work-rec
                   release work-rec
                   read sort-in
                       at end move high-value to loop-flag
                   end-read
           end-perform
           close sort-in
           .
    
          ******************************************************************
           sort-writer.
           move low-value to loop-flag
           open output sort-out
           return sort-work
               at end move high-value to loop-flag
           end-return
           perform
               until loop-flag = high-value
                   move work-rec to out-rec
                   write out-rec end-write
                   RETURN sort-work
                       at end move high-value to loop-flag
                   end-return
           end-perform
           close sort-out
           .
    
           exit program.
           end program sorting.


