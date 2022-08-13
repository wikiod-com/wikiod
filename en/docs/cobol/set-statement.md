---
title: "SET statement"
slug: "set-statement"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The COBOL `SET` statement sets values, and operating environment data.  It can be argued that `SET` was overused by the committee, as it has over a dozen documented syntax formats.

[![SET statement syntax diagrams][1]][1]


  [1]: https://i.stack.imgur.com/DYlHc.png

## SET pointer example
    SET handle TO returned-pointer
    SET handle UP BY LENGTH(returned-pointer)
    SET ADDRESS OF buffer-space TO handle
    MOVE buffer-space TO work-store
    DISPLAY "Second element is " work-store

