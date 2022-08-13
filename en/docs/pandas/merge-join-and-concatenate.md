---
title: "Merge, join, and concatenate"
slug: "merge-join-and-concatenate"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Syntax
- DataFrame.**merge**(*right, how='inner', on=None, left_on=None, right_on=None, left_index=False, right_index=False, sort=False, suffixes=('_x', '_y'), copy=True, indicator=False*)

- Merge DataFrame objects by performing a database-style join operation by columns or indexes.

- If joining columns on columns, the DataFrame indexes will be ignored. Otherwise if joining indexes on indexes or indexes on a column or columns, the index will be passed on.

## Parameters
| Parameters| Explanation |
| ------ | ------ |
|right | DataFrame|
|how | {‘left’, ‘right’, ‘outer’, ‘inner’}, default ‘inner’|
|left_on | label or list, or array-like. Field names to join on in left DataFrame. Can be a vector or list of vectors of the length of the DataFrame to use a particular vector as the join key instead of columns|
|right_on | label or list, or array-like. Field names to join on in right DataFrame or vector/list of vectors per left_on docs|
|left_index | boolean, default False. Use the index from the left DataFrame as the join key(s). If it is a MultiIndex, the number of keys in the other DataFrame (either the index or a number of columns) must match the number of levels|
|right_index | boolean, default False. Use the index from the right DataFrame as the join key. Same caveats as left_index|
|sort | boolean, default Fals. Sort the join keys lexicographically in the result DataFrame|
|suffixes |2-length sequence (tuple, list, ...). Suffix to apply to overlapping column names in the left and right side, respectively|
|copy | boolean, default True. If False, do not copy data unnecessarily|
|indicator |boolean or string, default False. If True, adds a column to output DataFrame called “_merge” with information on the source of each row. If string, column with information on source of each row will be added to output DataFrame, and column will be named value of string. Information column is Categorical-type and takes on a value of “left_only” for observations whose merge key only appears in ‘left’ DataFrame, “right_only” for observations whose merge key only appears in ‘right’ DataFrame, and “both” if the observation’s merge key is found in both.|

## Merging two DataFrames
    In [1]: df1 = pd.DataFrame({'x': [1, 2, 3], 'y': ['a', 'b', 'c']})
    
    In [2]: df2 = pd.DataFrame({'y': ['b', 'c', 'd'], 'z': [4, 5, 6]})

    In [3]: df1
    Out[3]: 
       x  y
    0  1  a
    1  2  b
    2  3  c   

    In [4]: df2
    Out[4]: 
       y  z
    0  b  4
    1  c  5
    2  d  6

Inner join:
===========
Uses the intersection of keys from two DataFrames.

 
    In [5]: df1.merge(df2) # by default, it does an inner join on the common column(s)
    Out[5]: 
       x  y  z
    0  2  b  4
    1  3  c  5

Alternatively specify intersection of keys from two Dataframes.

    In [5]: merged_inner = pd.merge(left=df1, right=df2, left_on='y', right_on='y')
    Out[5]: 
       x  y  z
    0  2  b  4
    1  3  c  5

Outer join:
===========
Uses the union of the keys from two DataFrames.

    In [6]: df1.merge(df2, how='outer')
    Out[6]: 
         x  y    z
    0  1.0  a  NaN
    1  2.0  b  4.0
    2  3.0  c  5.0
    3  NaN  d  6.0

Left join:
==========
Uses only keys from left DataFrame.


    In [7]: df1.merge(df2, how='left')
    Out[7]: 
       x  y    z
    0  1  a  NaN
    1  2  b  4.0
    2  3  c  5.0

Right Join
==========

Uses only keys from right DataFrame.
 
    In [8]: df1.merge(df2, how='right')
    Out[8]: 
         x  y  z
    0  2.0  b  4
    1  3.0  c  5
    2  NaN  d  6



## What is the difference between join and merge
Consider the dataframes `left` and `right`

    left = pd.DataFrame([['a', 1], ['b', 2]], list('XY'), list('AB'))
    left

       A  B
    X  a  1
    Y  b  2

---
    right = pd.DataFrame([['a', 3], ['b', 4]], list('XY'), list('AC'))
    right

       A  C
    X  a  3
    Y  b  4

***`join`***  
Think of `join` as wanting to combine to dataframes based on their respective indexes.  If there are overlapping columns, `join` will want you to add a suffix to the overlapping column name from left dataframe.  Our two dataframes do have an overlapping column name `A`.

    left.join(right, lsuffix='_')

      A_  B  A  C
    X  a  1  a  3
    Y  b  2  b  4

Notice the index is preserved and we have 4 columns.  2 columns from `left` and 2 from `right`.

If the indexes did not align

    left.join(right.reset_index(), lsuffix='_', how='outer')

        A_    B index    A    C
    0  NaN  NaN     X    a  3.0
    1  NaN  NaN     Y    b  4.0
    X    a  1.0   NaN  NaN  NaN
    Y    b  2.0   NaN  NaN  NaN

I used an outer join to better illustrate the point.  If the indexes do not align, the result will be the union of the indexes.

We can tell `join` to use a specific column in the left dataframe to use as the join key, but it will still use the index from the right.

    left.reset_index().join(right, on='index', lsuffix='_')

      index A_  B  A  C
    0     X  a  1  a  3
    1     Y  b  2  b  4

***`merge`***  
Think of `merge` as aligning on columns.  By default `merge` will look for overlapping columns in which to merge on.  `merge` gives better control over merge keys by allowing the user to specify a subset of the overlapping columns to use with parameter `on`, or to separately allow the specification of which columns on the left and which columns on the right to merge by.

`merge` will return a combined dataframe in which the index will be destroyed.

This simple example finds the overlapping column to be `'A'` and combines based on it.

    left.merge(right)

       A  B  C
    0  a  1  3
    1  b  2  4

Note the index is `[0, 1]` and no longer `['X', 'Y']`

You can explicitly specify that you are merging on the index with the `left_index` or `right_index` paramter

    left.merge(right, left_index=True, right_index=True, suffixes=['_', ''])

      A_  B  A  C
    X  a  1  a  3
    Y  b  2  b  4

And this looks exactly like the `join` example above.

## Merge
For instance, two tables are given,

**T1**

    id    x        y
    8    42        1.9
    9    30        1.9

**T2**

    id    signal
    8    55
    8    56    
    8    59
    9    57
    9    58    
    9    60

The goal is to get the new table T3:

    id    x        y        s1        s2        s3
    8    42        1.9        55        56        58
    9    30        1.9        57        58        60

Which is to create columns `s1`, `s2` and `s3`, each corresponding to a row (the number of rows per `id` is always fixed and equal to 3)

By applying `join` (which takes an optional on argument which may be a column or multiple column names, which specifies that the passed DataFrame is to be aligned on that column in the DataFrame). So the solution can be as shown below:

df = df1.merge(df2.groupby('id')['signal'].apply(lambda x: x.reset_index(drop=True)).unstack().reset_index())
    
    df
    Out[63]: 
       id   x    y   0   1   2
    0   8  42  1.9  55  56  59
    1   9  30  1.9  57  58  60

If I separate them:

    df2t = df2.groupby('id')['signal'].apply(lambda x: x.reset_index(drop=True)).unstack().reset_index()
    
    df2t
    Out[59]: 
       id   0   1   2
    0   8  55  56  59
    1   9  57  58  60

    df = df1.merge(df2t)
    
    df
    Out[61]: 
       id   x    y   0   1   2
    0   8  42  1.9  55  56  59
    1   9  30  1.9  57  58  60

## Merging / concatenating / joining multiple data frames (horizontally and vertically)
generate sample data frames:

    In [57]: df3 = pd.DataFrame({'col1':[211,212,213], 'col2': [221,222,223]})
    
    In [58]: df1 = pd.DataFrame({'col1':[11,12,13], 'col2': [21,22,23]})
    
    In [59]: df2 = pd.DataFrame({'col1':[111,112,113], 'col2': [121,122,123]})
    
    In [60]: df3 = pd.DataFrame({'col1':[211,212,213], 'col2': [221,222,223]})
    
    In [61]: df1
    Out[61]:
       col1  col2
    0    11    21
    1    12    22
    2    13    23
    
    In [62]: df2
    Out[62]:
       col1  col2
    0   111   121
    1   112   122
    2   113   123
    
    In [63]: df3
    Out[63]:
       col1  col2
    0   211   221
    1   212   222
    2   213   223

merge / join / concatenate data frames [df1, df2, df3] vertically - add rows

    In [64]: pd.concat([df1,df2,df3], ignore_index=True)
    Out[64]:
       col1  col2
    0    11    21
    1    12    22
    2    13    23
    3   111   121
    4   112   122
    5   113   123
    6   211   221
    7   212   222
    8   213   223

merge / join / concatenate data frames horizontally (aligning by index):

    In [65]: pd.concat([df1,df2,df3], axis=1)
    Out[65]:
       col1  col2  col1  col2  col1  col2
    0    11    21   111   121   211   221
    1    12    22   112   122   212   222
    2    13    23   113   123   213   223



## Merge, Join and Concat
**Merging key names are same**

    pd.merge(df1, df2, on='key')

**Merging key names are different**

    pd.merge(df1, df2, left_on='l_key', right_on='r_key')

**Different types of joining**

    pd.merge(df1, df2, on='key', how='left')

**Merging on multiple keys**

    pd.merge(df1, df2, on=['key1', 'key2'])

**Treatment of overlapping columns**

    pd.merge(df1, df2, on='key', suffixes=('_left', '_right'))

**Using row index instead of merging keys**

    pd.merge(df1, df2, right_index=True, left_index=True)

Avoid use of `.join` syntax as it gives exception for overlapping columns

**Merging on left dataframe index and right dataframe column**

    pd.merge(df1, df2, right_index=True, left_on='l_key')

**Concate dataframes**

Glued vertically

    pd.concat([df1, df2, df3], axis=0)

Glued horizontally

    pd.concat([df1, df2, df3], axis=1)

