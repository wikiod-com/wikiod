---
title: "Creating a data.table"
slug: "creating-a-datatable"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A data.table is an enhanced version of the data.frame class from base R. As such, its `class()` attribute is the vector `"data.table" "data.frame"` and functions that work on a data.frame will also work with a data.table. There are many ways to create, load or coerce to a data.table, as seen here.

## Coerce a data.frame
To copy a data.frame as a data.table, use `as.data.table` or `data.table`:

    DF = data.frame(x = letters[1:5], y = 1:5, z = (1:5) > 3)

    DT <- as.data.table(DF)
    # or
    DT <- data.table(DF)

This is rarely necessary. One exception is when using built-in datasets like `mtcars`, which must be copied since they cannot be modified in-place.




## Build with data.table()
There is a constructor of the same name:

    DT <- data.table(
      x = letters[1:5], 
      y = 1:5, 
      z = (1:5) > 3
    )
    #    x y     z
    # 1: a 1 FALSE
    # 2: b 2 FALSE
    # 3: c 3 FALSE
    # 4: d 4  TRUE
    # 5: e 5  TRUE

Unlike `data.frame`, `data.table` will not coerce strings to factors by default:

    sapply(DT, class)
    #               x           y           z 
    #     "character"   "integer"   "logical" 


## Read in with fread()
We can read from a text file:

    dt <- fread("my_file.csv")

Unlike `read.csv`, `fread` will read strings as strings, not as factors by default.

See the [topic on `fread`][need_a_link] for more examples.


## Modify a data.frame with setDT()
For efficiency, data.table offers a way of altering a data.frame or list to make a data.table in-place:

    # example data.frame
    DF = data.frame(x = letters[1:5], y = 1:5, z = (1:5) > 3)

    # modification
    setDT(DF)
    
Note that we do not `<-` assign the result, since the object `DF` has been modified in-place. 

The class attributes of the data.frame will be retained:

    sapply(DF, class)
    #         x         y         z 
    #  "factor" "integer" "logical" 


## Copy another data.table with copy()
    # example data
    DT1 = data.table(x = letters[1:2], y = 1:2, z = (1:2) > 3)


Due to the way data.tables are manipulated, `DT2 <- DT1` will *not* make a copy. That is, later modifications to the columns or other attributes of `DT2` will affect `DT1` as well. When you want a real copy, use

    DT2 = copy(DT1)

To see the difference, here's what happens without a copy:
    
    DT2 <- DT1
    DT2[, w := 1:2]
    
    DT1
    #    x y     z w
    # 1: a 1 FALSE 1
    # 2: b 2 FALSE 2
    DT2
    #    x y     z w
    # 1: a 1 FALSE 1
    # 2: b 2 FALSE 2

And with a copy:    

    DT2 <- copy(DT1)
    DT2[, w := 1:2]

    DT1
    #    x y     z
    # 1: a 1 FALSE
    # 2: b 2 FALSE
    DT2
    #    x y     z w
    # 1: a 1 FALSE 1
    # 2: b 2 FALSE 2

So the changes do not propagate in the latter case.

