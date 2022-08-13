---
title: "Counting unique cells"
slug: "counting-unique-cells"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Using COUNTIF()
    =SUMPRODUCT((A1:A100<>"")/COUNTIF(A1:A100,A1:A100&""))

counts *unique cell values* within A1:A100, *excluding* blank cells and ones with an empty string ("").

How does it do that? Example: 

    A1:A100 =    [1,   1,   2,   "apple", "peach", "apple", "", "", -,  -,  -,  ...]

Adding *&""* to the array is needed to turn blank cells (-) into empty strings (""). Result:

    A1:A100&"" = ["1", "1", "2", "apple", "peach", "apple", "", "", "", "", "", ...]

After this trick, COUNTIF() can be applied. Both "" and - are counted as the same:

    COUNTIF(A1:A100,A1:A100&"") = [2, 2, 1, 2, 1, 2, 94, 94, 94, 94, 94, ...]

To get the count of all unique cells, excluding blanks and "", we can divide

    (A1:A100<>""), which is [1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, ...]

by our intermediate result, `COUNTIF(A1:A100,A1:A100&"")`, and sum up over the values.

    SUMPRODUCT((A1:A100<>"")/COUNTIF(A1:A100,A1:A100&""))  
    = (1/2 + 1/2 + 1/1 + 1/2 + 1/1 + 1/2 + 0/94 + 0/94 + 0/94 + 0/94 + 0/94 + ...)
    = 4

## Using FREQUENCY() and MATCH()
    =SUMPRODUCT(IF(FREQUENCY(MATCH(A1:A100,A1:A100,0),MATCH(A1:A100,A1:A100,0))>0,1))

