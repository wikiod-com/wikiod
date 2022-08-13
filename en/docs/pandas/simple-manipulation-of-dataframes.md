---
title: "Simple manipulation of DataFrames"
slug: "simple-manipulation-of-dataframes"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Delete a column in a DataFrame
There are a couple of ways to delete a column in a DataFrame.

    import numpy as np
    import pandas as pd
    
    np.random.seed(0)
    
    pd.DataFrame(np.random.randn(5, 6), columns=list('ABCDEF'))
    
    print(df)
    # Output:
    #           A         B         C         D         E         F
    # 0 -0.895467  0.386902 -0.510805 -1.180632 -0.028182  0.428332
    # 1  0.066517  0.302472 -0.634322 -0.362741 -0.672460 -0.359553
    # 2 -0.813146 -1.726283  0.177426 -0.401781 -1.630198  0.462782
    # 3 -0.907298  0.051945  0.729091  0.128983  1.139401 -1.234826
    # 4  0.402342 -0.684810 -0.870797 -0.578850 -0.311553  0.056165
    
**1) Using `del`**
    
    del df['C']

    print(df)
    # Output:
    #           A         B         D         E         F
    # 0 -0.895467  0.386902 -1.180632 -0.028182  0.428332
    # 1  0.066517  0.302472 -0.362741 -0.672460 -0.359553
    # 2 -0.813146 -1.726283 -0.401781 -1.630198  0.462782
    # 3 -0.907298  0.051945  0.128983  1.139401 -1.234826
    # 4  0.402342 -0.684810 -0.578850 -0.311553  0.056165
    
**2) Using `drop`**
    
    df.drop(['B', 'E'], axis='columns', inplace=True)
    # or df = df.drop(['B', 'E'], axis=1) without the option inplace=True

    print(df)
    # Output:
    #           A         D         F
    # 0 -0.895467 -1.180632  0.428332
    # 1  0.066517 -0.362741 -0.359553
    # 2 -0.813146 -0.401781  0.462782
    # 3 -0.907298  0.128983 -1.234826
    # 4  0.402342 -0.578850  0.056165

**3) Using `drop` with column numbers**

To use column integer numbers instead of names (remember column indices start at zero):

    df.drop(df.columns[[0, 2]], axis='columns')

    print(df)
    # Output:
    #           D
    # 0 -1.180632
    # 1 -0.362741
    # 2 -0.401781
    # 3  0.128983
    # 4 -0.578850

## Adding a new column
    df = pd.DataFrame({'A': [1, 2, 3], 'B': [4, 5, 6]})
    
    print(df)
    # Output: 
    #    A  B
    # 0  1  4
    # 1  2  5
    # 2  3  6

## Directly assign ##

    df['C'] = [7, 8, 9]

    print(df)
    # Output: 
    #    A  B  C
    # 0  1  4  7
    # 1  2  5  8
    # 2  3  6  9

## Add a constant column ##

    df['C'] = 1

    print(df)

    # Output: 
    #    A  B  C
    # 0  1  4  1
    # 1  2  5  1
    # 2  3  6  1

## Column as an expression in other columns ##

    df['C'] = df['A'] + df['B']
    
    # print(df)
    # Output: 
    #    A  B  C
    # 0  1  4  5
    # 1  2  5  7
    # 2  3  6  9

    df['C'] = df['A']**df['B']

    print(df)
    # Output:
    #    A  B    C
    # 0  1  4    1
    # 1  2  5   32
    # 2  3  6  729

Operations are computed component-wise, so if we would have columns as lists

    a = [1, 2, 3]
    b = [4, 5, 6]

the column in the last expression would be obtained as

    c = [x**y for (x,y) in zip(a,b)]

    print(c)
    # Output:
    # [1, 32, 729]

## Create it on the fly ##

    df_means = df.assign(D=[10, 20, 30]).mean()
    
    print(df_means)
    # Output: 
    # A     2.0
    # B     5.0
    # C     7.0
    # D    20.0  # adds a new column D before taking the mean
    # dtype: float64

## add multiple columns ##

    df = pd.DataFrame({'A': [1, 2, 3], 'B': [4, 5, 6]})
    df[['A2','B2']] = np.square(df)
    
    print(df)
    # Output:
    #    A  B  A2  B2
    # 0  1  4   1  16
    # 1  2  5   4  25
    # 2  3  6   9  36

## add multiple columns on the fly ##

    new_df = df.assign(A3=df.A*df.A2, B3=5*df.B)

    print(new_df)
    # Output:
    #    A  B  A2  B2  A3  B3
    # 0  1  4   1  16   1  20
    # 1  2  5   4  25   8  25
    # 2  3  6   9  36  27  30



## Rename a column
    df = pd.DataFrame({'old_name_1': [1, 2, 3], 'old_name_2': [5, 6, 7]})

    print(df)
    # Output: 
    #    old_name_1  old_name_2
    # 0           1           5
    # 1           2           6
    # 2           3           7

To rename one or more columns, pass the old names and new names as a dictionary:

    df.rename(columns={'old_name_1': 'new_name_1', 'old_name_2': 'new_name_2'}, inplace=True)
    print(df)
    # Output:
    #   new_name_1  new_name_2
    # 0           1           5
    # 1           2           6
    # 2           3           7

Or a function:

    df.rename(columns=lambda x: x.replace('old_', '_new'), inplace=True)
    print(df)
    # Output:
    #   new_name_1  new_name_2
    # 0           1           5
    # 1           2           6
    # 2           3           7

You can also set `df.columns` as the list of the new names:

    df.columns = ['new_name_1','new_name_2']
    print(df)
    # Output:
    #   new_name_1  new_name_2
    # 0           1           5
    # 1           2           6
    # 2           3           7

More details [can be found here][1].

  [1]: http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.rename.html



## Locate and replace data in a column
    import pandas as pd

    df = pd.DataFrame({'gender': ["male", "female","female"],
                        'id': [1, 2, 3] })
    >>> df
       gender  id
    0    male   1
    1  female   2
    2  female   3
            

To encode the male to 0 and female to 1:

    df.loc[df["gender"] == "male","gender"] = 0
    df.loc[df["gender"] == "female","gender"] = 1

    >>> df
           gender  id
        0       0   1
        1       1   2
        2       1   3

     



## Adding a new row to DataFrame
Given a DataFrame:

    s1 = pd.Series([1,2,3])
    s2 = pd.Series(['a','b','c'])
    
    df = pd.DataFrame([list(s1), list(s2)],  columns =  ["C1", "C2", "C3"])
    print df

Output:

      C1 C2 C3
    0  1  2  3
    1  a  b  c

Lets add a new row, `[10,11,12]`:

    df = pd.DataFrame(np.array([[10,11,12]]), \
            columns=["C1", "C2", "C3"]).append(df, ignore_index=True)
    print df

Output:

       C1  C2  C3
    0  10  11  12
    1   1   2   3
    2   a   b   c


## Delete / drop rows from DataFrame
let's generate a DataFrame first:

    df = pd.DataFrame(np.arange(10).reshape(5,2), columns=list('ab'))
    
    print(df) 
    # Output:
    #    a  b
    # 0  0  1
    # 1  2  3
    # 2  4  5
    # 3  6  7
    # 4  8  9

drop rows with indexes: `0` and `4` using `drop([...], inplace=True)` method:

    df.drop([0,4], inplace=True)
    
    print(df)
    # Output
    #    a  b
    # 1  2  3
    # 2  4  5
    # 3  6  7

drop rows with indexes: `0` and `4` using `df = drop([...])` method:

    df = pd.DataFrame(np.arange(10).reshape(5,2), columns=list('ab'))
    
    df = df.drop([0,4])
    
    print(df)
    # Output:
    #    a  b
    # 1  2  3
    # 2  4  5
    # 3  6  7

using negative selection method:

    df = pd.DataFrame(np.arange(10).reshape(5,2), columns=list('ab'))

    df = df[~df.index.isin([0,4])]
    
    print(df)
    # Output:
    #    a  b
    # 1  2  3
    # 2  4  5
    # 3  6  7





## Reorder columns
    # get a list of columns
    cols = list(df)
    
    # move the column to head of list using index, pop and insert
    cols.insert(0, cols.pop(cols.index('listing')))
    
    # use ix to reorder
    df2 = df.ix[:, cols]

