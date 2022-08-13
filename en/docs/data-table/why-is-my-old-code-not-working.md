---
title: "Why is my old code not working?"
slug: "why-is-my-old-code-not-working"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

The data.table package has undergone a number of changes and innovations over time. Here are some potential pitfalls that can help users looking at legacy code or reviewing old blog posts.

## unique and duplicated no longer works on keyed data.table
*This is for those moving to data.table >= 1.9.8*


You have a data set of pet owners and names, but you suspect some repeated data has been captured.
 
    library(data.table)
    DT <- data.table(pet = c("dog","dog","cat","dog"),
                     owner = c("Alice","Bob","Charlie","Alice"),
                     entry.date = c("31/12/2015","31/12/2015","14/2/2016","14/2/2016"),
                     key = "owner")


    > tables()
         NAME NROW NCOL MB COLS                 KEY  
    [1,] DT      4    3  1 pet,owner,entry.date owner
    Total: 1MB

Recall keying a table will sort it. Alice has been entered twice.

    > DT
       pet   owner entry.date
    1: dog   Alice 31/12/2015
    2: dog   Alice  14/2/2016
    3: dog     Bob 31/12/2015
    4: cat Charlie  14/2/2016


Say you used `unique` to get rid of duplicates in your data based on the key, using the most recent data capture date by setting fromLast to TRUE.

<!-- if version [lt 1.9.8] -->
    clean.DT <- unique(DT, fromLast = TRUE)

    > tables()
        NAME     NROW NCOL MB COLS                 KEY  
    [1,] clean.DT    3    3  1 pet,owner,entry.date owner
    [2,] DT          4    3  1 pet,owner,entry.date owner
    Total: 2MB
<!-- end version if -->

Alice duplicate been removed.

<!-- if version [gte 1.9.8] -->
    clean.DT <- unique(DT, fromLast = TRUE)

    > tables()
         NAME     NROW NCOL MB COLS                 KEY  
    [1,] clean.DT    4    3  1 pet,owner,entry.date owner
    [2,] DT          4    3  1 pet,owner,entry.date owner
<!-- end version if -->

This does not work. Still 4 rows!

# Fix

Use the `by=` parameter **which no longer defaults to your key** but to all columns.

    clean.DT <- unique(DT, by = key(DT), fromLast = TRUE)

Now all is well.

    > clean.DT
       pet   owner entry.date
    1: dog   Alice  14/2/2016
    2: dog     Bob 31/12/2015
    3: cat Charlie  14/2/2016

# Details and stopgap fix

See [item 1 in the NEWS release notes][1] for details:

> Changes in v1.9.8 (on CRAN 25 Nov 2016)
>
> POTENTIALLY BREAKING CHANGES
>
> 1. By default all columns are now used by `unique()`, `duplicated()` and
> `uniqueN()` data.table methods, #1284 and #1841. To restore old
> behaviour: `options(datatable.old.unique.by.key=TRUE)`. In 1 year this
> option to restore the old default will be deprecated with warning. In
> 2 years the option will be removed. Please explicitly pass `by=key(DT)`
> for clarity. Only code that relies on the default is affected. 266
> CRAN and Bioconductor packages using data.table were checked before
> release. 9 needed to change and were notified. Any lines of code
> without test coverage will have been missed by these checks. Any
> packages not on CRAN or Bioconductor were not checked.
> 

So you can use the options as a temporary workaround until your code is fixed.

    options(datatable.old.unique.by.key=TRUE)


  [1]: https://github.com/Rdatatable/data.table/blob/master/NEWS.md#changes-in-v198--on-cran-25-nov-2016

