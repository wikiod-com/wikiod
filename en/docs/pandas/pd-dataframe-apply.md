---
title: "pd.DataFrame.apply"
slug: "pddataframeapply"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## pandas.DataFrame.apply Basic Usage
The pandas.DataFrame.apply() method is used to apply a given function to an entire `DataFrame` --- for example, computing the square root of every entry of a given `DataFrame` or summing across each row of a `DataFrame` to return a `Series`.

The below is a basic example of usage of this function:

    # create a random DataFrame with 7 rows and 2 columns
    df = pd.DataFrame(np.random.randint(0,100,size = (7,2)), 
                      columns = ['fst','snd'])

    >>> df
       fst  snd
    0   40   94
    1   58   93
    2   95   95
    3   88   40
    4   25   27
    5   62   64
    6   18   92
    
    # apply the square root function to each column:
    # (this returns a DataFrame where each entry is the sqrt of the entry in df;
    # setting axis=0 or axis=1 doesn't make a difference)
    >>> df.apply(np.sqrt)
            fst       snd
    0  6.324555  9.695360
    1  7.615773  9.643651
    2  9.746794  9.746794
    3  9.380832  6.324555
    4  5.000000  5.196152
    5  7.874008  8.000000
    6  4.242641  9.591663

    # sum across the row (axis parameter now makes a difference):
    >>> df.apply(np.sum, axis=1)
    0    134
    1    151
    2    190
    3    128
    4     52
    5    126
    6    110
    dtype: int64

    >>> df.apply(np.sum)
    fst    386
    snd    505
    dtype: int64

