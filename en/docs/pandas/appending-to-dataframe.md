---
title: "Appending to DataFrame"
slug: "appending-to-dataframe"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Appending a new row to DataFrame
    In [1]: import pandas as pd
    
    In [2]: df = pd.DataFrame(columns = ['A', 'B', 'C'])
    
    In [3]: df
    Out[3]: 
    Empty DataFrame
    Columns: [A, B, C]
    Index: []

Appending a row by a single column value:

    In [4]: df.loc[0, 'A'] = 1
    
    In [5]: df
    Out[5]: 
       A    B    C
    0  1  NaN  NaN

Appending a row, given list of values:

    In [6]: df.loc[1] = [2, 3, 4]
    
    In [7]: df
    Out[7]: 
       A    B    C
    0  1  NaN  NaN
    1  2    3    4

Appending a row given a dictionary:

    In [8]: df.loc[2] = {'A': 3, 'C': 9, 'B': 9}
    
    In [9]: df
    Out[9]: 
       A    B    C
    0  1  NaN  NaN
    1  2    3    4
    2  3    9    9

The first input in .loc[] is the index. If you use an existing index, you will overwrite the values in that row:

    In [17]: df.loc[1] = [5, 6, 7]
    
    In [18]: df
    Out[18]: 
       A    B    C
    0  1  NaN  NaN
    1  5    6    7
    2  3    9    9

    
    In [19]: df.loc[0, 'B'] = 8
    
    In [20]: df
    Out[20]: 
       A  B    C
    0  1  8  NaN
    1  5  6    7
    2  3  9    9



## Append a DataFrame to another DataFrame
Let us assume we have the following two DataFrames:


    In [7]: df1
    Out[7]: 
        A   B
    0  a1  b1
    1  a2  b2
    
    In [8]: df2
    Out[8]: 
        B   C
    0  b1  c1

The two DataFrames are not required to have the same set of columns. The append method does not change either of the original DataFrames. Instead, it returns a new DataFrame by appending the original two. Appending a DataFrame to another one is quite simple:

    In [9]: df1.append(df2)
    Out[9]: 
         A   B    C
    0   a1  b1  NaN
    1   a2  b2  NaN
    0  NaN  b1   c1

As you can see, it is possible to have duplicate indices (0 in this example). To avoid this issue, you may ask Pandas to reindex the new DataFrame for you:

    In [10]: df1.append(df2, ignore_index = True)
    Out[10]: 
         A   B    C
    0   a1  b1  NaN
    1   a2  b2  NaN
    2  NaN  b1   c1




