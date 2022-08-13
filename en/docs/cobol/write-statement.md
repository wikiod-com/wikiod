---
title: "WRITE statement"
slug: "write-statement"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

The `WRITE` statement releases logical records to an `output` or `input-output` storage resource, and for logical positioning of lines within a logical page.

WRITE sequential

[![WRITE sequential syntax diagram][1]][1]

WRITE random

[![WRITE random syntax diagram][2]][2]


  [1]: https://i.stack.imgur.com/iKYKy.png
  [2]: https://i.stack.imgur.com/4lqh5.png

## WRITE examples
    WRITE record-buff
    
    WRITE indexed-record
        WITH LOCK
        ON INVALID KEY
            DISPLAY "Key exists, REWRITING..." END-DISPLAY
            PERFORM rewrite-key
    END-WRITE
    IF indexed-file-status NOT EQUAL ZERO THEN
        DISPLAY "Write problem: " indexed-file-status UPON SYSERR
        END-DISPLAY
        PERFORM evasive-manoeuvres
    END-IF
    
    WRITE record-name-1 AFTER ADVANCING PAGE
    
    WRITE record-name-1 FROM header-record-1
        AFTER ADVANCING 2 LINES
        AT END-OF-PAGE
            PERFORM write-page-header
            PERFORM write-last-detail-reminder
    END-WRITE


