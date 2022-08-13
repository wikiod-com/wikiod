---
title: "Index Match for Excel"
slug: "index-match-for-excel"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

A more versatile alternative to VLOOKUP. An Index Match packs the power of a Vlookup and Hlookup in one formula. You also do not need to know which number column/row the information is. Due to this, deleting columns/rows will not mess up the formula.



## Vertical Index Match
Vertical Index Match
[![Vert Index Match][1]][1]


  [1]: https://i.stack.imgur.com/Xbn5l.png

    =INDEX(A1:A3,MATCH(E2,C1:C3,0))

## Horizontal Index Match
Horizontal Index Match
[![Horizontal Index Match][1]][1]


  [1]: https://i.stack.imgur.com/4YFdX.png

    =INDEX(A1:C1,MATCH(E2,A3:C3,0))

