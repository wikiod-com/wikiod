---
title: "Using .SD and .SDcols for the subset of data"
slug: "using-sd-and-sdcols-for-the-subset-of-data"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

The special symbol `.SD` is available in `j` of `DT[i,j,by]`, capturing the **S**ubset of **D**ata for each `by` group surviving the filter, `i`. `.SDcols` is a helper. Type ``?`special-symbols` `` for the official docs.

 A reminder: `DT[where, select|update|do, by]` syntax is used to work with columns of a data.table.
 - The "where" part is the `i` argument
 - The "select|update|do" part is the `j` argument
 
 These two arguments are usually passed by position instead of by name.

## Using .SD and .SDcols
# .SD

`.SD` refers to the subset of the `data.table` for each group, excluding all columns used in `by`. 

`.SD` along with `lapply` can be used to apply any function to multiple columns by group in a `data.table`

 We will continue using the same built-in dataset, `mtcars`: 

    mtcars = data.table(mtcars) # Let's not include rownames to keep things simpler

Mean of all columns in the dataset by *number of cylinders*, `cyl`: 

    mtcars[ , lapply(.SD, mean), by = cyl]

    #   cyl      mpg     disp        hp     drat       wt     qsec        vs        am     gear     carb
    #1:   6 19.74286 183.3143 122.28571 3.585714 3.117143 17.97714 0.5714286 0.4285714 3.857143 3.428571
    #2:   4 26.66364 105.1364  82.63636 4.070909 2.285727 19.13727 0.9090909 0.7272727 4.090909 1.545455
    #3:   8 15.10000 353.1000 209.21429 3.229286 3.999214 16.77214 0.0000000 0.1428571 3.285714 3.500000


Apart from `cyl`, there are other categorical columns in the dataset such as `vs`, `am`, `gear` and `carb`. It doesn't really make sense to take the `mean` of these columns. So let's exclude these columns. This is where `.SDcols` comes into the picture. 

# .SDcols  

`.SDcols` specifies the columns of the `data.table` that are included in `.SD`. 

Mean of all columns (continuous columns) in the dataset by *number of gears* `gear`, and *number of cylinders*, `cyl`, arranged by `gear` and `cyl`:

    # All the continuous variables in the dataset
    cols_chosen <- c("mpg", "disp", "hp", "drat", "wt", "qsec")

    mtcars[order(gear, cyl), lapply(.SD, mean), by = .(gear, cyl), .SDcols = cols_chosen]
    
    #   gear cyl    mpg     disp       hp     drat       wt    qsec
    #1:    3   4 21.500 120.1000  97.0000 3.700000 2.465000 20.0100
    #2:    3   6 19.750 241.5000 107.5000 2.920000 3.337500 19.8300
    #3:    3   8 15.050 357.6167 194.1667 3.120833 4.104083 17.1425
    #4:    4   4 26.925 102.6250  76.0000 4.110000 2.378125 19.6125
    #5:    4   6 19.750 163.8000 116.5000 3.910000 3.093750 17.6700
    #6:    5   4 28.200 107.7000 102.0000 4.100000 1.826500 16.8000
    #7:    5   6 19.700 145.0000 175.0000 3.620000 2.770000 15.5000
    #8:    5   8 15.400 326.0000 299.5000 3.880000 3.370000 14.5500


Maybe we don't want to calculate the `mean` by groups. To calculate the mean for all the cars in the dataset, we don't specify the `by` variable. 

    mtcars[ , lapply(.SD, mean), .SDcols = cols_chosen] 

    #        mpg     disp       hp     drat      wt     qsec
    #1: 20.09062 230.7219 146.6875 3.596563 3.21725 17.84875

Note: It is not necessary to define `cols_chosen` beforehand. `.SDcols` can directly take column names

