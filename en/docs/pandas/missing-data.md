---
title: "Missing Data"
slug: "missing-data"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Should we include the non-documented `ffill` and `bfill`?

## Filling missing values
    In [11]: df = pd.DataFrame([[1, 2, None, 3], [4, None, 5, 6], 
                                [7, 8, 9, 10], [None, None, None, None]])
    
    Out[11]: 
         0    1    2     3
    0  1.0  2.0  NaN   3.0
    1  4.0  NaN  5.0   6.0
    2  7.0  8.0  9.0  10.0
    3  NaN  NaN  NaN   NaN

## Fill missing values with a single value:

    In [12]: df.fillna(0)
    Out[12]: 
         0    1    2     3
    0  1.0  2.0  0.0   3.0
    1  4.0  0.0  5.0   6.0
    2  7.0  8.0  9.0  10.0
    3  0.0  0.0  0.0   0.0   

This returns a new DataFrame. If you want to change the original DataFrame, either use the `inplace` parameter (`df.fillna(0, inplace=True)`) or assign it back to original DataFrame (`df = df.fillna(0)`).

## Fill missing values with the previous ones:

    In [13]: df.fillna(method='pad')  # this is equivalent to both method='ffill' and .ffill()
    Out[13]: 
         0    1    2     3
    0  1.0  2.0  NaN   3.0
    1  4.0  2.0  5.0   6.0
    2  7.0  8.0  9.0  10.0
    3  7.0  8.0  9.0  10.0

## Fill with the next ones:

    In [14]: df.fillna(method='bfill')  # this is equivalent to .bfill()
    Out[14]: 
         0    1    2     3
    0  1.0  2.0  5.0   3.0
    1  4.0  8.0  5.0   6.0
    2  7.0  8.0  9.0  10.0
    3  NaN  NaN  NaN   NaN

## Fill using another DataFrame:

    In [15]: df2 = pd.DataFrame(np.arange(100, 116).reshape(4, 4))
             df2
    Out[15]: 
         0    1    2    3
    0  100  101  102  103
    1  104  105  106  107
    2  108  109  110  111
    3  112  113  114  115

    In [16]: df.fillna(df2) #  takes the corresponding cells in df2 to fill df
    Out[16]: 
           0      1      2      3
    0    1.0    2.0  102.0    3.0
    1    4.0  105.0    5.0    6.0
    2    7.0    8.0    9.0   10.0
    3  112.0  113.0  114.0  115.0

## Dropping missing values
When creating a DataFrame `None` (python's missing value) is converted to `NaN` (pandas' missing value):

    In [11]: df = pd.DataFrame([[1, 2, None, 3], [4, None, 5, 6], 
                                [7, 8, 9, 10], [None, None, None, None]])
    
    Out[11]: 
         0    1    2     3
    0  1.0  2.0  NaN   3.0
    1  4.0  NaN  5.0   6.0
    2  7.0  8.0  9.0  10.0
    3  NaN  NaN  NaN   NaN

## Drop rows if at least one column has a missing value ##

    In [12]: df.dropna()
    Out[12]:
         0    1    2     3
    2  7.0  8.0  9.0  10.0

This returns a new DataFrame. If you want to change the original DataFrame, either use the `inplace` parameter (`df.dropna(inplace=True)`) or assign it back to original DataFrame (`df = df.dropna()`)</sub>.

## Drop rows if all values in that row are missing ##

    In [13]: df.dropna(how='all')
    Out[13]: 
         0    1    2     3
    0  1.0  2.0  NaN   3.0
    1  4.0  NaN  5.0   6.0
    2  7.0  8.0  9.0  10.0

## Drop *columns* that don't have at least 3 non-missing values ##

    In [14]: df.dropna(axis=1, thresh=3)
    Out[14]: 
         0     3
    0  1.0   3.0
    1  4.0   6.0
    2  7.0  10.0
    3  NaN   NaN


## Checking for missing values
In order to check whether a value is NaN, `isnull()` or `notnull()` functions can be used.

    In [1]: import numpy as np
    In [2]: import pandas as pd
    In [3]: ser = pd.Series([1, 2, np.nan, 4])
    In [4]: pd.isnull(ser)
    Out[4]: 
    0    False
    1    False
    2     True
    3    False
    dtype: bool   

Note that `np.nan == np.nan` returns False so you should avoid comparison against np.nan:

    In [5]: ser == np.nan
    Out[5]: 
    0    False
    1    False
    2    False
    3    False
    dtype: bool

Both functions are also defined as methods on Series and DataFrames. 

    In [6]: ser.isnull()
    Out[6]: 
    0    False
    1    False
    2     True
    3    False
    dtype: bool

Testing on DataFrames:

    In [7]: df = pd.DataFrame({'A': [1, np.nan, 3], 'B': [np.nan, 5, 6]})
    In [8]: print(df)
    Out[8]: 
         A    B
    0  1.0  NaN
    1  NaN  5.0
    2  3.0  6.0    

    In [9]: df.isnull()  # If the value is NaN, returns True.
    Out[9]: 
           A      B
    0  False   True
    1   True  False
    2  False  False

    In [10]: df.notnull()  # Opposite of .isnull(). If the value is not NaN, returns True.
    Out[10]: 
           A      B
    0   True  False
    1  False   True
    2   True   True

## Interpolation
    import pandas as pd
    import numpy as np
    
    df = pd.DataFrame({'A':[1,2,np.nan,3,np.nan],
                       'B':[1.2,7,3,0,8]})
    
    df['C'] = df.A.interpolate()
    df['D'] = df.A.interpolate(method='spline', order=1)

    print (df)
         A    B    C         D
    0  1.0  1.2  1.0  1.000000
    1  2.0  7.0  2.0  2.000000
    2  NaN  3.0  2.5  2.428571
    3  3.0  0.0  3.0  3.000000
    4  NaN  8.0  3.0  3.714286

