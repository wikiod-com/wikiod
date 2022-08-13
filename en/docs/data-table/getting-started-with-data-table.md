---
title: "Getting started with data.table"
slug: "getting-started-with-datatable"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and setup
Install the stable release from CRAN:

    install.packages("data.table")       

Or the development version from github:

    install.packages("data.table", type = "source", 
      repos = "http://Rdatatable.github.io/data.table")

To revert from devel to CRAN, the current version must first be removed:

    remove.packages("data.table")
    install.packages("data.table")

[Visit the website for][1] full installation instructions and the latest version numbers. 

# Using the package

Usually you will want to load the package and all of its functions with a line like 

    library(data.table)

If you only need one or two functions, you can refer to them like `data.table::fread` instead. 


  [1]: https://github.com/Rdatatable/data.table/wiki/Installation

## Syntax and features
# Basic syntax

`DT[where, select|update|do, by]` syntax is used to work with columns of a data.table.
 - The "where" part is the `i` argument
 - The "select|update|do" part is the `j` argument
 
These two arguments are usually passed by position instead of by name.

A sequence of steps can be chained like `DT[...][...]`.

# Shortcuts, special functions and special symbols inside `DT[...]`
| Function or symbol | Notes |
| ------  | ------ |
| `.()`   | in several arguments, replaces `list()`
| `J()`   | in `i`, replaces `list()`
| `:=`    | in `j`, a function used to add or modify columns
| `.N`    | in `i`, the total number of rows <br> in `j`, the number of rows in a group
| `.I`    | in `j`, the vector of row numbers in the table (filtered by `i`)
| `.SD`   | in `j`, the current subset of the data <br> selected by the `.SDcols` argument
| `.GRP`  | in `j`, the current index of the subset of the data
| `.BY`   | in `j`, the list of by values for the current subset of data
| `V1, V2, ...`  | default names for unnamed columns created in `j`

# Joins inside `DT[...]` 

| Notation | Notes |
| ------  | ------ |
| `DT1[DT2, on, j]`  | join two tables
| `i.*`  | special prefix on DT2's columns after the join
| `by=.EACHI`  | special option available only with a join
| `DT1[!DT2, on, j]`  | anti-join two tables
| `DT1[DT2, on, roll, j]`  | join two tables, rolling on the last column in `on=`

# Reshaping, stacking and splitting

| Notation | Notes |
| ------  | ------ |
| `melt(DT, id.vars, measure.vars)`  | transform to long format  <br> for multiple columns, use `measure.vars = patterns(...)`
| `dcast(DT, formula)`  | transform to wide format
| `rbind(DT1, DT2, ...)`  | stack enumerated data.tables
| `rbindlist(DT_list, idcol)`  | stack a list of data.tables 
| `split(DT, by)`  | split a data.table into a list 

# Some other functions specialized for data.tables

| Function(s) | Notes |
| ------  | ------ |
| `foverlaps` | overlap joins
| `merge` | another way of joining two tables
| `set` | another way of adding or modifying columns
| `fintersect`, `fsetdiff`, <br> `funion`, `fsetequal`, <br> `unique`, `duplicated`, `anyDuplicated` | set-theory operations with rows as elements  
| `CJ`| the Cartesian product of vectors
| `uniqueN`  | the number of distinct rows
| `rowidv(DT, cols)`  | row ID (1 to .N) within each group determined by cols
| `rleidv(DT, cols)`  | group ID (1 to .GRP) within each group determined by runs of cols
| `shift(DT, n)` | apply a shift operator to every column
| `setorder`, `setcolorder`, <br> `setnames`, `setkey`, `setindex`, <br> `setattr` | modify attributes and order by reference

# Other features of the package

| Features | Notes |
| ------  | ------ |
| `IDate` and `ITime` | integer dates and times

## Getting started and finding help
The package's [official wiki][2] has some essential materials: 
- As a new user, you will want to check out the [vignettes, FAQ and cheat sheet][1].

- Before asking a question -- here on StackOverflow or anywhere else -- please read [the support page][3].

For help on individual functions, the syntax is `help("fread")` or `?fread`. If the package has not been loaded, use the full name like `?data.table::fread`.

  [1]: https://github.com/Rdatatable/data.table/wiki/Getting-started
  [2]: https://github.com/Rdatatable/data.table/wiki
  [3]: https://github.com/Rdatatable/data.table/wiki/Support

