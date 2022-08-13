---
title: "Subsetting rows by group"
slug: "subsetting-rows-by-group"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

 A reminder: `DT[where, select|update|do, by]` syntax is used to work with columns of a data.table.
 - The "where" part is the `i` argument
 - The "select|update|do" part is the `j` argument
 
 These two arguments are usually passed by position instead of by name.

## Selecting rows within each group
    # example data
    DT <- data.table(Titanic)

Suppose that, for each sex, we want the rows with the highest survival numbers:

    DT[Survived == "Yes", .SD[ N == max(N) ], by=Sex]

    #    Class    Sex   Age Survived   N
    # 1:  Crew   Male Adult      Yes 192
    # 2:   1st Female Adult      Yes 140


`.SD` is the subset of data associated with each `Sex`; and we are subsetting this further, to the rows that meet our condition. If speed is important, instead use [an approach suggested by eddi on SO][1]:

    DT[ DT[Survived == "Yes", .I[ N == max(N) ], by=Sex]$V1 ]

    #    Class    Sex   Age Survived   N
    # 1:  Crew   Male Adult      Yes 192
    # 2:   1st Female Adult      Yes 140

# Pitfalls

In the last line of code, `.I` refers to the row numbers of the full data.table. However, [this is not true when there is no `by`][2]:

    DT[ Survived == "Yes", .I]

    # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16

    DT[ Survived == "Yes", .I, by=Sex]$I

    # 17 18 19 20 25 26 27 28 21 22 23 24 29 30 31 32


  [1]: http://stackoverflow.com/a/16574176/
  [2]: https://github.com/Rdatatable/data.table/issues/1494

## Selecting groups
    # example data
    DT = data.table(Titanic)

Suppose we only want to see second class:

    DT[ Class == "2nd" ]

    #    Class    Sex   Age Survived   N
    # 1:   2nd   Male Child       No   0
    # 2:   2nd Female Child       No   0
    # 3:   2nd   Male Adult       No 154
    # 4:   2nd Female Adult       No  13
    # 5:   2nd   Male Child      Yes  11
    # 6:   2nd Female Child      Yes  13
    # 7:   2nd   Male Adult      Yes  14
    # 8:   2nd Female Adult      Yes  80

Here, we simply subset the data using `i`, the "where" clause.


## Selecting groups by condition
    # example data
    DT = data.table(Titanic)

Suppose we want to see each class only if a majority survived:

    DT[, if (sum(N[Survived=="Yes"]) > sum(N[Survived=="No"]) ) .SD, by=Class]

    #    Class    Sex   Age Survived   N
    # 1:   1st   Male Child       No   0
    # 2:   1st Female Child       No   0
    # 3:   1st   Male Adult       No 118
    # 4:   1st Female Adult       No   4
    # 5:   1st   Male Child      Yes   5
    # 6:   1st Female Child      Yes   1
    # 7:   1st   Male Adult      Yes  57
    # 8:   1st Female Adult      Yes 140

Here, we return the subset of data `.SD` only if our condition is met. An alternative is

    DT[, .SD[ sum(N[Survived=="Yes"]) > sum(N[Survived=="No"]) ) ], by=Class]

but this has sometimes proven slower.

