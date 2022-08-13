---
title: "Cleaning data"
slug: "cleaning-data"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Handling duplicates
    # example data
    DT = data.table(id = c(1,2,2,3,3,3))[, v := LETTERS[.I]][]

To deal with "duplicates," combine [counting rows in a group][2] and [subsetting rows by group][1]. 

# Keep one row per group

Aka "drop duplicates" aka "deduplicate" aka "uniquify."

    unique(DT, by="id")
    # or
    DT[, .SD[1L], by=id]
    #    id v
    # 1:  1 A
    # 2:  2 B
    # 3:  3 D

This keeps the first row. To select a different row, one can fiddle with the `1L` part or use `order` in `i`.

# Keep only unique rows
    
    DT[, if (.N == 1L) .SD, by=id]
    #    id v
    # 1:  1 A

# Keep only nonunique rows

    DT[, if (.N > 1L) .SD, by=id]
    #    id v
    # 1:  2 B
    # 2:  2 C
    # 3:  3 D
    # 4:  3 E
    # 5:  3 F

  [1]: https://www.wikiod.com/data-table/subsetting-rows-by-group
  [2]: https://www.wikiod.com/data-table/computing-summary-statistics#Counting rows by group



