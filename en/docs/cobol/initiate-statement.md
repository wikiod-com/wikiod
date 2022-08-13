---
title: "INITIATE statement"
slug: "initiate-statement"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

The `INITIATE` statement initializes internal `Report Writer` control fields. Most of a report writer setup occurs in the `DATA DIVISION` with very brief `PROCEDURE DIVISION` statements.  Once initialized, `GENERATE` does all the hard work of control break and paging of reports. 

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/r9M6T.png

## INITIATE reporting control variables
    INITIATE report-1 report-2

