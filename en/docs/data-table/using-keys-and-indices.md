---
title: "Using keys and indices"
slug: "using-keys-and-indices"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The key and indices of a data.table allow certain computations to run faster, mostly related to joins and subsetting. The key describes the table's current sort order; while each index stores information about the order of the table with respect a sequence of columns. 

See the "Remarks" section below for links to the official vignettes on the topic.

The official vignettes are the best introduction to this topic:

- ["Keys and fast binary search based subset"][1]
- ["Secondary indices and auto indexing"][2] 


# Keys vs indices

A data.table can be "keyed" by a sequence of columns, telling interested functions that the data is sorted by those columns. To get or set the key, use the functions documented at `?key`.

Similarly, functions can take advantage of a data.table's "indices." Each index -- and a table can have more than one -- stores information about the order of the data with respect a sequence of columns. Like a key, an index can speed up certain tasks. To get or set indices, use the functions documented at `?indices`. 

Indices may also be set automatically (currently only for a single column at a time). See `?datatable.optimize` for details on how this works and how to disable it if necessary.

# Verification and updating

Missing values are allowed in a key column.

Keys and indices are stored as attributes and may, by accident, not correspond to the actual order of data in the table. Many functions check the validity of the key or index before using it, but it's worth keeping in mind.

Keys and indices are removed after updates where it's not obvious that sort order is preserved. For example, starting from `DT = data.table(a=c(1,2,4), key="a")`, if we update like `DT[2, a := 3]`, the key is broken.


  [1]: https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-keys-fast-subset.html
  [2]: https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-secondary-indices-and-auto-indexing.html

## Improving performance for selecting subsets
    # example data
    set.seed(1)
    n  = 1e7
    ng = 1e4
    DT = data.table(
        g1  = sample(ng, n, replace=TRUE),
        g2  = sample(ng, n, replace=TRUE),
        v  = rnorm(n)
    )

# Matching on one column

After the first run of a subsetting operation with `==` or `%in%`...

    system.time(
        DT[ g1 %in% 1:100]
    )
    #    user  system elapsed 
    #    0.12    0.03    0.16 

An index has been created automatically for `g1`. Subsequent subsetting operations run almost instantly:

    system.time(
        DT[ g1 %in% 1:100]
    )
    #    user  system elapsed 
    #       0       0       0

To monitor when an index is created or used, add the `verbose=TRUE` option or change the global setting `options(datatable.verbose=TRUE)`.

# Matching on multiple columns

Currently, matching on two columns does not automatically create an index:

    system.time(
        DT[ g1 %in% 1:100 & g2 %in% 1:100]
    )
    #    user  system elapsed 
    #    0.57    0.00    0.57

Re-run this and it will remain slow. Even if we manually add the index with `setindex(DT, g1, g2)`, it will remain slow because this query is not yet optimized by the package. 

Fortunately, if we can enumerate the combinations of values we want to search for and an index is available, we can quickly equi-join:

    system.time(
        DT[ CJ(g1 = 1:100, g2 = 1:100, unique=TRUE), on=.(g1, g2), nomatch=0]
    )
    #    user  system elapsed 
    #    0.53    0.00    0.54 
    setindex(DT, g1, g2)
    system.time(
        DT[ CJ(g1 = 1:100, g2 = 1:100, unique=TRUE), on=.(g1, g2), nomatch=0]
    )
    #    user  system elapsed 
    #       0       0       0

With `CJ`, it's important to watch out for the number of combinations becoming too large.

