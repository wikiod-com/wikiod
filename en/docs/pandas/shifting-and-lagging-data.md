---
title: "Shifting and Lagging Data"
slug: "shifting-and-lagging-data"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Shifting or lagging values in a dataframe
    import pandas as pd

    df = pd.DataFrame({'eggs': [1,2,4,8,], 'chickens': [0,1,2,4,]})

    df
    
    #    chickens  eggs
    # 0         0     1
    # 1         1     2
    # 2         2     4
    # 3         4     8

    df.shift()

    #    chickens  eggs
    # 0       NaN   NaN
    # 1       0.0   1.0
    # 2       1.0   2.0
    # 3       2.0   4.0

    df.shift(-2)

    #    chickens  eggs
    # 0       2.0   4.0
    # 1       4.0   8.0
    # 2       NaN   NaN
    # 3       NaN   NaN

    df['eggs'].shift(1) - df['chickens']
    
    # 0    NaN
    # 1    0.0
    # 2    0.0
    # 3    0.0

The first argument to [`.shift()`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.shift.html) is `periods`, the number of spaces to move the data.  If not specified, defaults to `1`.

