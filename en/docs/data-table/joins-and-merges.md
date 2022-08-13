---
title: "Joins and merges"
slug: "joins-and-merges"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

A join combines two tables containing related columns. The term covers a wide range of operations, essentially everything except [appending the two tables](https://www.wikiod.com/data-table/reshaping-stacking-and-splitting#Stacking multiple tables using rbindlist). "Merge" is a synonym. Type ``?`[.data.table` `` for the official docs.

## Syntax
 - x[i, on, j]   
       # join: data.table x & data.table or list i 
 - x[!i, on, j]  
       # anti-join

# Working with keyed tables

If `x` & `i` have a [key][1] or `x` is keyed to match `i`'s first few columns, then the `on` can be skipped like `x[i]`.

# Disambiguating column names in common

In `j` of `x[i, on, j]`, columns of `i` can be referred with `i.*` prefixes.

# Grouping on subsets

In `j` of `x[i, on, j, by=.EACHI]`, `j` is computed for each row of `i`. 

This is the only value of `by` worth using. For any other value, columns of `i` are not available.


  [1]: https://www.wikiod.com/data-table/using-keys-and-indices

## Update values in a join
When data is ["tidy,"][1] it is often organized into several tables. To combine the data for analysis, we need to "update" one table with values from another.

For example, we might have sales data for performances, where attributes of the performer (their budget) and of the location (its population) are stored in separate tables:

    set.seed(1)
    mainDT = data.table(
      p_id = rep(LETTERS[1:2], c(2,4)), 
      geo_id = sample(rep(state.abb[c(1,25,50)], 3:1)), 
      sales = sample(100, 6)
    )
    pDT   = data.table(id = LETTERS[1:2], budget = c(60, 75))
    geoDT = data.table(id = state.abb[c(1,50)], pop = c(100, 200))

    mainDT # sales data
    #    p_id geo_id sales
    # 1:    A     AL    95
    # 2:    A     WY    66
    # 3:    B     AL    62
    # 4:    B     MO     6
    # 5:    B     AL    20
    # 6:    B     MO    17


    pDT # performer attributes
    #    id budget
    # 1:  A     60
    # 2:  B     75

    geoDT # location attributes
    #    id pop
    # 1: AL 100
    # 2: WY 200

When we are ready to do some analysis, we need to grab variables from these other tables:

    DT = copy(mainDT)

    DT[pDT, on=.(p_id = id), budget := i.budget]
    DT[geoDT, on=.(geo_id = id), pop := i.pop]

    #    p_id geo_id sales budget pop
    # 1:    A     AL    95     60 100
    # 2:    A     WY    66     60 200
    # 3:    B     AL    62     75 100
    # 4:    B     MO     6     75  NA
    # 5:    B     AL    20     75 100
    # 6:    B     MO    17     75  NA

A `copy` is taken to avoid contaminating the raw data, but we could work directly on `mainDT` instead.

# Advantages to using separate tables

The advantages of this structure are covered in the paper on tidy data, but in this context:

1. *Tracing missing data.* Only rows that match up in the merge receive an assignment. We have no data for `geo_id == "MO"` above, so its variables are `NA` in our final table. If we see missing data like this unexpectedly, we can trace it back to the missing observation in the `geoDT` table and investigate from there whether we have a data problem that can be addressed.

2. *Comprehensibility.* In building our statistical model, it might be important to keep in mind that `budget` is constant for each performer. In general, understanding the structure of the data pays dividends.

3. *Memory size.* There might be a large number of performer and location attributes that don't end up in the statistical model. This way, we don't need to include them in the (possibly massive) table used for analysis.

# Programmatically determining columns

If there are many columns in `pDT`, but we only want to select a few, we can use

    p_cols = "budget"
    DT[pDT, on=.(p_id = id), (p_cols) := mget(sprintf("i.%s", p_cols))]

The parentheses around `(p_cols) :=` are essential, as noted in [the doc on creating columns][2].


  [1]: https://www.jstatsoft.org/article/view/v059i10
  [2]: https://www.wikiod.com/data-table/adding-and-modifying-columns

## Equi-join

    # example data
    a = data.table(id = c(1L, 1L, 2L, 3L, NA_integer_), x = 11:15)
    #    id  x
    # 1:  1 11
    # 2:  1 12
    # 3:  2 13
    # 4:  3 14
    # 5: NA 15

    b = data.table(id = 1:2, y = -(1:2))
    #    id  y
    # 1:  1 -1
    # 2:  2 -2

# Intuition

Think of `x[i]` as selecting a subset of `x` for each row of `i`. This syntax mirrors matrix subsetting in base R and is consistent with the first argument meaning "where", in [`DT[where, select|update|do, by]`][1].

One might wonder why this new syntax is worth learning, since `merge(x,i)` still works with data.tables. The short answer is that it we usually wants to merge and then do something further. The `x[i]` syntax concisely captures this pattern of use and also allows for more efficient computation. For a more detailed explanation, read FAQs [1.12][2] and [2.14][3].

# Handling multiply-matched rows

By default, every row of `a` matching each row of `b` is returned:

    a[b, on="id"]
    #    id  x  y
    # 1:  1 11 -1
    # 2:  1 12 -1
    # 3:  2 13 -2

This can be tweaked with `mult`:

    a[b, on="id", mult="first"]
    #    id  x  y
    # 1:  1 11 -1
    # 2:  2 13 -2

# Handling unmatched rows

By default, unmatched rows of `a` still show up in the result:

    b[a, on="id"]
    #    id  y  x
    # 1:  1 -1 11
    # 2:  1 -1 12
    # 3:  2 -2 13
    # 4:  3 NA 14
    # 5: NA NA 15

To hide these, use `nomatch`:

    b[a, on="id", nomatch=0]
    #    id  y  x
    # 1:  1 -1 11
    # 2:  1 -1 12
    # 3:  2 -2 13

Note that `x[i]` will attempt to match NAs in `i`.

# Counting matches returned

To count the number of matches for each row of `i`, use `.N` and `by=.EACHI`.

    b[a, on="id", .N, by=.EACHI]
    #    id N
    # 1:  1 1
    # 2:  1 1
    # 3:  2 1
    # 4:  3 0
    # 5: NA 0


  [1]: https://www.wikiod.com/data-table/getting-started-with-datatable#Syntax and features
  [2]: https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-faq.html#MergeDiff
  [3]: https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-faq.html#can-you-explain-further-why-data.table-is-inspired-by-ab-syntax-in-base

