---
title: "Computing summary statistics"
slug: "computing-summary-statistics"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

A reminder: `DT[where, select|update|do, by]` syntax is used to work with columns of a data.table. 
- The "where" part is the `i` argument
- The "select|update|do" part is the `j` argument

These two arguments are usually passed by position instead of by name.

## Applying a summarizing function to multiple variables
    # example data
    DT = data.table(iris)
    DT[, Bin := cut(Sepal.Length, c(4,6,8))]

To apply the same summarizing function to every column by group, we can use `lapply` and `.SD`

    DT[, lapply(.SD, median), by=.(Species, Bin)]

    #       Species   Bin Sepal.Length Sepal.Width Petal.Length Petal.Width
    # 1:     setosa (4,6]          5.0         3.4         1.50         0.2
    # 2: versicolor (6,8]          6.4         2.9         4.60         1.4
    # 3: versicolor (4,6]          5.6         2.7         4.05         1.3
    # 4:  virginica (6,8]          6.7         3.0         5.60         2.1
    # 5:  virginica (4,6]          5.8         2.7         5.00         1.9

We can filter the columns in `.SD` with the `.SDcols` argument:

    DT[, lapply(.SD, median), by=.(Species, Bin), .SDcols="Petal.Length"]

    #       Species   Bin Petal.Length
    # 1:     setosa (4,6]         1.50
    # 2: versicolor (6,8]         4.60
    # 3: versicolor (4,6]         4.05
    # 4:  virginica (6,8]         5.60
    # 5:  virginica (4,6]         5.00

# Multiple summarizing functions

Currently, the simplest extension to multiple functions is perhaps:

    DT[, unlist(recursive=FALSE, lapply(
        .(med = median, iqr = IQR),
        function(f) lapply(.SD, f)
    )), by=.(Species, Bin), .SDcols=Petal.Length:Petal.Width]

    #       Species   Bin med.Petal.Length med.Petal.Width iqr.Petal.Length iqr.Petal.Width
    # 1:     setosa (4,6]             1.50             0.2            0.175           0.100
    # 2: versicolor (6,8]             4.60             1.4            0.300           0.200
    # 3: versicolor (4,6]             4.05             1.3            0.525           0.275
    # 4:  virginica (6,8]             5.60             2.1            0.700           0.500
    # 5:  virginica (4,6]             5.00             1.9            0.200           0.200

If you want the names to be like `Petal.Length.med` instead of `med.Petal.Length`, change the order:

    DT[, unlist(recursive=FALSE, lapply(
        .SD,
        function(x) lapply(.(med = median, iqr = IQR), function(f) f(x))
    )), by=.(Species, Bin), .SDcols=Petal.Length:Petal.Width]

    #       Species   Bin Petal.Length.med Petal.Length.iqr Petal.Width.med Petal.Width.iqr
    # 1:     setosa (4,6]             1.50            0.175             0.2           0.100
    # 2: versicolor (6,8]             4.60            0.300             1.4           0.200
    # 3: versicolor (4,6]             4.05            0.525             1.3           0.275
    # 4:  virginica (6,8]             5.60            0.700             2.1           0.500
    # 5:  virginica (4,6]             5.00            0.200             1.9           0.200



## Counting rows by group
    # example data
    DT = data.table(iris)
    DT[, Bin := cut(Sepal.Length, c(4,6,8))]

# Using `.N`

`.N` in `j` stores the number of rows in a subset.  When exploring data, `.N` is handy to... 

1. count rows in a group,

       DT[Species == "setosa", .N]

       # 50

1. or count rows in all groups,

       DT[, .N, by=.(Species, Bin)]

       #       Species   Bin  N
       # 1:     setosa (4,6] 50
       # 2: versicolor (6,8] 20
       # 3: versicolor (4,6] 30
       # 4:  virginica (6,8] 41
       # 5:  virginica (4,6]  9

1. or find groups that have a certain number of rows.

       DT[, .N, by=.(Species, Bin)][ N < 25 ]

       #       Species   Bin  N
       # 1: versicolor (6,8] 20
       # 2:  virginica (4,6]  9

# Handling missing groups

However, we are missing groups with a count of zero above. If they matter, we can use `table` from base:

    DT[, data.table(table(Species, Bin))][ N < 25 ]

    #       Species   Bin  N
    # 1:  virginica (4,6]  9
    # 2:     setosa (6,8]  0
    # 3: versicolor (6,8] 20

Alternately, we can join on all groups:

    DT[CJ(Species=Species, Bin=Bin, unique=TRUE), on=c("Species","Bin"), .N, by=.EACHI][N < 25]

    #       Species   Bin  N
    # 1:     setosa (6,8]  0
    # 2: versicolor (6,8] 20
    # 3:  virginica (4,6]  9

A note on `.N`:
 - This example uses `.N` in `j`, where it refers to size of a subset. 
 - In `i`, it refers to the total number of rows.

## Custom summaries
    # example data
    DT = data.table(iris)
    DT[, Bin := cut(Sepal.Length, c(4,6,8))]

Suppose we want the `summary` function output for `Sepal.Length` along with the number of observations:

    DT[, c(
        as.list(summary(Sepal.Length)),
        N = .N
    ), by=.(Species, Bin)]

    #       Species   Bin Min. 1st Qu. Median  Mean 3rd Qu. Max.  N
    # 1:     setosa (4,6]  4.3     4.8    5.0 5.006     5.2  5.8 50
    # 2: versicolor (6,8]  6.1     6.2    6.4 6.450     6.7  7.0 20
    # 3: versicolor (4,6]  4.9     5.5    5.6 5.593     5.8  6.0 30
    # 4:  virginica (6,8]  6.1     6.4    6.7 6.778     7.2  7.9 41
    # 5:  virginica (4,6]  4.9     5.7    5.8 5.722     5.9  6.0  9

We have to make `j` a list of columns. Usually, some playing around with `c`, `as.list` and `.` is enough to figure out the correct way to proceed.

# Assigning summary statistics as new columns

Instead of making a summary table, we may want to store a summary statistic in a new column. We can use `:=` as usual. For example,

    DT[, is_big := .N >= 25, by=.(Species, Bin)]

# Pitfalls 

## Untidy data

If you find yourself wanting to parse column names, like

> Take the mean of `x.Length/x.Width` where `x` takes ten different values.

then you are probably looking at data embedded in column names, which is a bad idea. Read about [tidy data][1] and then reshape to long format.

## Rowwise summaries

Data frames and data.tables are well-designed for tabular data, where rows correspond to observations and columns to variables. If you find yourself wanting to summarize over rows, like 

> Find the standard deviation across columns for each row. 

then you should probably be using a matrix or some other data format entirely.

  [1]: https://www.jstatsoft.org/article/view/v059i10/


## The summary function
    # example data
    DT = data.table(iris)
    DT[, Bin := cut(Sepal.Length, c(4,6,8))]

`summary` is handy for browsing summary statistics. Besides direct usage like `summary(DT)`, it can also be applied per-group conveniently with `split`:

    lapply(split(DT, by=c("Species", "Bin"), drop=TRUE, keep.by=FALSE), summary)

    # $`setosa.(4,6]`
    #   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    #  Min.   :4.300   Min.   :2.300   Min.   :1.000   Min.   :0.100  
    #  1st Qu.:4.800   1st Qu.:3.200   1st Qu.:1.400   1st Qu.:0.200  
    #  Median :5.000   Median :3.400   Median :1.500   Median :0.200  
    #  Mean   :5.006   Mean   :3.428   Mean   :1.462   Mean   :0.246  
    #  3rd Qu.:5.200   3rd Qu.:3.675   3rd Qu.:1.575   3rd Qu.:0.300  
    #  Max.   :5.800   Max.   :4.400   Max.   :1.900   Max.   :0.600  
    # 
    # $`versicolor.(6,8]`
    #   Sepal.Length   Sepal.Width    Petal.Length    Petal.Width  
    #  Min.   :6.10   Min.   :2.20   Min.   :4.000   Min.   :1.20  
    #  1st Qu.:6.20   1st Qu.:2.80   1st Qu.:4.400   1st Qu.:1.30  
    #  Median :6.40   Median :2.90   Median :4.600   Median :1.40  
    #  Mean   :6.45   Mean   :2.89   Mean   :4.585   Mean   :1.42  
    #  3rd Qu.:6.70   3rd Qu.:3.10   3rd Qu.:4.700   3rd Qu.:1.50  
    #  Max.   :7.00   Max.   :3.30   Max.   :5.000   Max.   :1.70  
    # 
    # [...results truncated...]

To include zero-count groups, set `drop=FALSE` in `split`.

