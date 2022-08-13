---
title: "Adding and modifying columns"
slug: "adding-and-modifying-columns"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The official vignette, ["Reference semantics"][1], is the best introduction to this topic.

A reminder: `DT[where, select|update|do, by]` syntax is used to work with columns of a data.table.
 - The "where" part is the `i` argument
 - The "select|update|do" part is the `j` argument
 
 These two arguments are usually passed by position instead of by name.

All modifications to columns can be done in `j`. Additionally, the `set` function is available for this use.


  [1]: https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-reference-semantics.html

## Editing values
    # example data
    DT = as.data.table(mtcars, keep.rownames = TRUE)

# Editing a column

Use the `:=` operator inside `j` to create new columns or modify existing ones:

    DT[, mpg_sq := mpg^2]

# Editing on a subset of rows

Use the `i` argument to subset to rows "where" edits should be made:

    DT[1:3, newvar := "Hello"]

As in a data.frame, we can subset using row numbers or logical tests. It is also possible to use [a "join" in `i` when modifying][need_a_link].

# Removing a column

Remove columns by setting to `NULL`:

    DT[, mpg_sq := NULL]

Note that we do not `<-` assign the result, since `DT` has been modified in-place.



# Editing multiple columns

Add multiple columns by using the `:=` operator's multivariate format:

    DT[, `:=`(mpg_sq = mpg^2, wt_sqrt = sqrt(wt))]
    # or 
    DT[, c("mpg_sq", "wt_sqrt") := .(mpg^2, sqrt(wt))]

The `.()` syntax is used when the right-hand side of `LHS := RHS` is a list of columns.

# Editing multiple sequentially-dependent columns

If the columns are dependent and must be defined in sequence, some ways to do that are:

    DT[, c("mpg_sq", "mpg2_hp") := .(temp1 <- mpg^2, temp1/hp)]
    # or
    DT[, c("mpg_sq", "mpg2_hp") := {temp1 = mpg^2; .(temp1, temp1/hp)}]

# Editing columns by dynamically-determined names

For dynamically-determined column names, use parentheses:

    vn = "mpg_sq"
    DT[, (vn) := mpg^2]

# Using `set`

Columns can also be modified with `set` for a small reduction in overhead, though this is rarely necessary:

    set(DT, j = "hp_over_wt", v = mtcars$hp/mtcars$wt)


## Reordering columns
    # example data
    DT = as.data.table(mtcars, keep.rownames = TRUE)

To rearrange the order of columns, use `setcolorder`. For example, to reverse them

    setcolorder(DT, rev(names(DT)))

This costs almost nothing in terms of performance, since it is just permuting the list of column pointers in the data.table.

## Renaming columns
    # example data
    DT = as.data.table(mtcars, keep.rownames = TRUE)

To rename a column (while keeping its data the same), there is no need to copy the data to a column with a new name and delete the old one. Instead, we can use

    setnames(DT, "mpg_sq", "mpq_squared")

to modify the original column by reference.

## Modifying factor levels and other column attributes
    # example data
    DT = data.table(iris)

To modify factor levels by reference, use `setattr`:

    setattr(DT$Species, "levels", c("set", "ver", "vir")
    # or
    DT[, setattr(Species, "levels", c("set", "ver", "vir"))]

The second option might print the result to the screen.

With `setattr`, we avoid the copy usually incurred when doing `levels(x) <- lvls`, but it will also skip some checks, so it is important to be careful to assign a valid vector of levels.

