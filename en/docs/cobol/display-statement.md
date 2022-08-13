---
title: "DISPLAY statement"
slug: "display-statement"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

The `DISPLAY` statement causes data to be transferred to hardware or software of the operating environment.  `DISPLAY` comes in two forms, `UPON device` or for display of `SCREEN` data.  Environment variables can also be set with `DISPLAY UPON` in some implementations of COBOL, along with other extensions for data transfer of graphics or other device specific needs.

[![DISPLAY statement syntax diagram][1]][1]


  [1]: http://i.stack.imgur.com/P7XKM.png

## DISPLAY UPON
    DISPLAY "An error occurred with " tracked-resource UPON SYSERR
    
    DISPLAY A, B, C UPON CONSOLE

    DISPLAY group-data UPON user-device
        ON EXCEPTION
            WRITE device-exception-notice
        NOT ON EXCEPTION
            WRITE device-usage-log
    END-DISPLAY

UPON CONSOLE is a default, rarely written.  Messages with DISPLAY are one way of debugging COBOL code, but many COBOL programs are transactional in nature, and might not ever interact with a human operator once a job is submitted.

