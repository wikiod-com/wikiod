---
title: "STRING statement"
slug: "string-statement"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The `STRING` statement concatenates the partial or complete contents of multiple fields into a single result.

[![STRING statement syntax diagram][1]][1]


  [1]: https://i.stack.imgur.com/C6THp.png

## STRING example for C strings
    *> Strip off trailing zero bytes
    STRING c-string DELIMITED BY LOW-VALUE INTO working-store


