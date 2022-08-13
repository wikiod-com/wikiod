---
title: "RELEASE statement"
slug: "release-statement"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The `RELEASE` statement is used to give records to the COBOL `SORT` algorithm under programmer controlled conditions.

[![RELEASE statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/nsojp.png

## RELEASE a record to a SORT INPUT PROCEDURE
This is a contrived sample.  It sorts records based on an `ALPHABET` that has upper and lower case characters together, with `A` and `a` swapped compared to the other letters.  This was done on purpose to demonstrate the possibilities.  The SORT algorithm reader retrieves records using `RELEASE` in the `INPUT PROCEDURE`.  The `OUTPUT PROCEDURE` uses `RETURN` for the `SORT` algorithm writer.

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
          * This sets up a sort order lower/upper except for "A" and "a"
           special-names.
               alphabet mixed is " AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTu
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
               input procedure is sort-transform
               output procedure is output-uppercase.
    
           display sort-return.
           goback.
    
          ******************************************************************
           sort-transform.
           move low-value to loop-flag
           open input sort-in
           read sort-in
               at end move high-value to loop-flag
           end-read
           perform
               until loop-flag = high-value
                   move in-rec to work-rec
                   RELEASE work-rec
                   read sort-in
                       at end move high-value to loop-flag
                   end-read
           end-perform
           close sort-in
           .
    
          ******************************************************************
           output-uppercase.
           move low-value to loop-flag
           open output sort-out
           return sort-work
               at end move high-value to loop-flag
           end-return
           perform
               until loop-flag = high-value
                   move work-rec to out-rec
                   write out-rec end-write
                   return sort-work
                       at end move high-value to loop-flag
                   end-return
           end-perform
           close sort-out
           .
    
           exit program.
           end program sorting.

