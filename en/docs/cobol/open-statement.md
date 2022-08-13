---
title: "OPEN statement"
slug: "open-statement"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

The COBOL `OPEN` statement initiates file processing.  File resources in COBOL are defined in the `ENVIRONMENT DIVISION`, named in `FD` (File Descriptor) paragraphs.  These fd names are used to access physical disk files and various options are specified in a `SELECT` clauses in the `FILE-CONTROL` paragraph of the `INPUT-OUTPUT SECTION`. A programmer is expected to test a `FILE STATUS` identifier for status and error codes. 

Modes include `INPUT`, `OUTPUT`, `I-O` and `EXTEND`.

[![OPEN statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/qto0e.png

## OPEN sample, with LINAGE mini report
    COBOL *****************************************************************
          * Example of LINAGE File Descriptor
          * Tectonics: $ cocb -x linage.cob
          *            $ ./linage <filename ["linage.cob"]>
          *            $ cat -n mini-report
          *****************************************************************
           IDENTIFICATION DIVISION.
           PROGRAM-ID. linage-demo.
    
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               select optional data-file assign to file-name
                   organization is line sequential
                   file status is data-file-status.
               select mini-report assign to "mini-report".
    
           DATA DIVISION.
           FILE SECTION.
           FD  data-file.
           01  data-record.
               88 endofdata        value high-values.
               02 data-line        pic x(80).
           FD  mini-report
               linage is 16 lines
                   with footing at 15
                   lines at top 2
                   lines at bottom 2.
           01  report-line         pic x(80).
    
           WORKING-STORAGE SECTION.
           01  command-arguments   pic x(1024).
           01  file-name           pic x(160).
           01  data-file-status    pic 99.
           01  lc                  pic 99.
           01  report-line-blank.
               02 filler           pic x(18) value all "*".
               02 filler           pic x(05) value spaces.
               02 filler           pic x(34)
                   VALUE "THIS PAGE INTENTIONALLY LEFT BLANK".
               02 filler           pic x(05) value spaces.
               02 filler           pic x(18) value all "*".
           01  report-line-data.
               02 body-tag         pic 9(6).
               02 line-3           pic x(74).
           01  report-line-header.
               02 filler           pic x(6) VALUE "PAGE: ".
               02 page-no          pic 9999.
               02 filler           pic x(24).
               02 filler           pic x(5) VALUE " LC: ".
               02 header-tag       pic 9(6).
               02 filler           pic x(23).
               02 filler           pic x(6) VALUE "DATE: ".
               02 page-date        pic x(6).
    
           01  page-count          pic 9999.
    
           PROCEDURE DIVISION.
    
           accept command-arguments from command-line end-accept.
           string
               command-arguments delimited by space
               into file-name
           end-string.
           if file-name equal spaces
               move "linage.cob" to file-name
           end-if.
    
           open input data-file.
           read data-file
               at end
                   display "File: " function trim(file-name) " open error"
                   go to early-exit
           end-read.
    
           open output mini-report.
    
           write report-line
               from report-line-blank
           end-write.
    
           move 1 to page-count.
           accept page-date from date end-accept.
           move page-count to page-no.
           write report-line
               from report-line-header
               after advancing page
           end-write.
    
           perform readwrite-loop until endofdata.
    
           display
               "Normal termination, file name: "
               function trim(file-name)
               " ending status: "
               data-file-status
           close mini-report.
    
          * Goto considered harmful?  Bah!  :)
           early-exit.
           close data-file.
           exit program.
           stop run.
    
          ****************************************************************
           readwrite-loop.
           move data-record to report-line-data
           move linage-counter to body-tag
           write report-line from report-line-data
               end-of-page
                   add 1 to page-count end-add
                   move page-count to page-no
                   move linage-counter to header-tag
                   write report-line from report-line-header
                       after advancing page
                   end-write
           end-write
           read data-file
               at end set endofdata to true
           end-read
           .
    
          *****************************************************************
          * Commentary
          * LINAGE is set at a 20 line logical page
          *  16 body lines
          *   2 top lines
          *   A footer line at 15 (inside the body count)
          *   2 bottom lines
          * Build with:
          * $ cobc -x -Wall -Wtruncate linage.cob
          * Evaluate with:
          * $ ./linage
          * This will read in linage.cob and produce a useless mini-report
          * $ cat -n mini-report
          *****************************************************************
           END PROGRAM linage-demo.


