---
title: "Getting information about DataFrames"
slug: "getting-information-about-dataframes"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## Get DataFrame information and memory usage
To get basic information about a DataFrame including the column names and datatypes:

    import pandas as pd

    df = pd.DataFrame({'integers': [1, 2, 3], 
                       'floats': [1.5, 2.5, 3], 
                       'text': ['a', 'b', 'c'], 
                       'ints with None': [1, None, 3]})

    df.info()
    <class 'pandas.core.frame.DataFrame'>
    Int64Index: 3 entries, 0 to 2
    Data columns (total 4 columns):
    floats            3 non-null float64
    integers          3 non-null int64
    ints with None    2 non-null float64
    text              3 non-null object
    dtypes: float64(2), int64(1), object(1)
    memory usage: 120.0+ bytes

To get the memory usage of the DataFrame:

    >>> df.info(memory_usage='deep')
    <class 'pandas.core.frame.DataFrame'>
    Int64Index: 3 entries, 0 to 2
    Data columns (total 4 columns):
    floats            3 non-null float64
    integers          3 non-null int64
    ints with None    2 non-null float64
    text              3 non-null object
    dtypes: float64(2), int64(1), object(1)
    memory usage: 234.0 bytes

## Dataframe's various summary statistics.
    import pandas as pd
    df = pd.DataFrame(np.random.randn(5, 5), columns=list('ABCDE'))

To generate various summary statistics. For numeric values the number of non-NA/null values (`count`), the mean (`mean`), the standard deviation `std` and values known as the [five-number summary](https://en.wikipedia.org/wiki/Five-number_summary) :

- `min`: minimum (smallest observation)
- `25%`: lower quartile or first quartile (Q1)
- `50%`: median (middle value, Q2)
- `75%`: upper quartile or third quartile (Q3)
- `max`: maximum (largest observation)


    >>> df.describe()
                  A         B         C         D         E
    count  5.000000  5.000000  5.000000  5.000000  5.000000
    mean  -0.456917 -0.278666  0.334173  0.863089  0.211153
    std    0.925617  1.091155  1.024567  1.238668  1.495219
    min   -1.494346 -2.031457 -0.336471 -0.821447 -2.106488
    25%   -1.143098 -0.407362 -0.246228 -0.087088 -0.082451
    50%   -0.536503 -0.163950 -0.004099  1.509749  0.313918
    75%    0.092630  0.381407  0.120137  1.822794  1.060268
    max    0.796729  0.828034  2.137527  1.891436  1.870520


## List DataFrame column names
    df = pd.DataFrame({'a': [1, 2, 3], 'b': [4, 5, 6], 'c': [7, 8, 9]})

To list the column names in a DataFrame:

    >>> list(df)
    ['a', 'b', 'c']

This list comprehension method is especially useful when using the debugger:

    >>> [c for c in df]
    ['a', 'b', 'c']

This is the long way:

    sampledf.columns.tolist()

You can also print them as an index instead of a list (this won't be very visible for dataframes with many columns though):

    df.columns

