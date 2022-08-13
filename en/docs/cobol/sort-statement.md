---
title: "SORT statement"
slug: "sort-statement"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

The COBOL `SORT` statement can be used to sort files and tables in working storage.

*SORT file*

[![SORT file syntax diagram][1]][1]

*SORT table*

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/iatOV.png
  [2]: https://i.stack.imgur.com/XZOrk.png

## Sorting standard in to standard out
    GCobol* GnuCOBOL SORT verb example using standard in and standard out
           identification division.
           program-id. sorting.

           environment division.
           input-output section.
           file-control.
               select sort-in
                   assign keyboard
                   organization line sequential.
               select sort-out
                   assign display
                   organization line sequential.
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

           procedure division.
           sort sort-work
               ascending key work-rec
               using  sort-in
               giving sort-out.

           goback.
           exit program.
           end program sorting.


