---
title: "Duplicated data"
slug: "duplicated-data"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Drop duplicated
Use [`drop_duplicates`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.drop_duplicates.html):

    In [216]: df = pd.DataFrame({'A':[1,2,3,3,2],
         ...:                    'B':[1,7,3,0,8]})
    
    In [217]: df
    Out[217]: 
       A  B
    0  1  1
    1  2  7
    2  3  3
    3  3  0
    4  2  8
    
    # keep only the last value
    In [218]: df.drop_duplicates(subset=['A'], keep='last')
    Out[218]: 
       A  B
    0  1  1
    3  3  0
    4  2  8
    
    # keep only the first value, default value
    In [219]: df.drop_duplicates(subset=['A'], keep='first')
    Out[219]: 
       A  B
    0  1  1
    1  2  7
    2  3  3
    
    # drop all duplicated values
    In [220]: df.drop_duplicates(subset=['A'], keep=False)
    Out[220]: 
       A  B
    0  1  1

When you don't want to get a copy of a data frame, but to modify the existing one:

    In [221]: df = pd.DataFrame({'A':[1,2,3,3,2],
         ...:                    'B':[1,7,3,0,8]})
    
    In [222]: df.drop_duplicates(subset=['A'], inplace=True)
    
    In [223]: df
    Out[223]: 
       A  B
    0  1  1
    1  2  7
    2  3  3

## Select duplicated
If need set value `0` to column `B`, where in column `A` are duplicated data first create mask by [`Series.duplicated`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.duplicated.html) and then use [`DataFrame.ix`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.ix.html) or [`Series.mask`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.mask.html):

    
    In [224]: df = pd.DataFrame({'A':[1,2,3,3,2],
         ...:                    'B':[1,7,3,0,8]})
    
    In [225]: mask = df.A.duplicated(keep=False)
    
    In [226]: mask
    Out[226]: 
    0    False
    1     True
    2     True
    3     True
    4     True
    Name: A, dtype: bool
    
    In [227]: df.ix[mask, 'B'] = 0
    
    In [228]: df['C'] = df.A.mask(mask, 0)
    
    In [229]: df
    Out[229]: 
       A  B  C
    0  1  1  1
    1  2  0  0
    2  3  0  0
    3  3  0  0
    4  2  0  0

If need invert mask use `~`:

    In [230]: df['C'] = df.A.mask(~mask, 0)
    
    In [231]: df
    Out[231]: 
       A  B  C
    0  1  1  0
    1  2  0  2
    2  3  0  3
    3  3  0  3
    4  2  0  2

## Counting and getting unique elements
Number of unique elements in a series:

    In [1]: id_numbers = pd.Series([111, 112, 112, 114, 115, 118, 114, 118, 112])
    In [2]: id_numbers.nunique()
    Out[2]: 5

Get unique elements in a series:

    In [3]: id_numbers.unique()
    Out[3]: array([111, 112, 114, 115, 118], dtype=int64)

    In [4]: df = pd.DataFrame({'Group': list('ABAABABAAB'), 
                               'ID': [1, 1, 2, 3, 3, 2, 1, 2, 1, 3]})

    In [5]: df
    Out[5]: 
      Group  ID
    0     A   1
    1     B   1
    2     A   2
    3     A   3
    4     B   3
    5     A   2
    6     B   1
    7     A   2
    8     A   1
    9     B   3

Number of unique elements in each group:

    In [6]: df.groupby('Group')['ID'].nunique()
    Out[6]: 
    Group
    A    3
    B    2
    Name: ID, dtype: int64

Get of unique elements in each group:

    In [7]: df.groupby('Group')['ID'].unique()
    Out[7]: 
    Group
    A    [1, 2, 3]
    B       [1, 3]
    Name: ID, dtype: object

## Get unique values from a column.
    In [15]: df = pd.DataFrame({"A":[1,1,2,3,1,1],"B":[5,4,3,4,6,7]})

    In [21]: df
    Out[21]: 
       A  B
    0  1  5
    1  1  4
    2  2  3
    3  3  4
    4  1  6
    5  1  7

To get unique values in column A and B.

    In [22]: df["A"].unique()
    Out[22]: array([1, 2, 3])

    In [23]: df["B"].unique()
    Out[23]: array([5, 4, 3, 6, 7])

To get the unique values in column A as a list (note that `unique()` can be used in two slightly different ways)

    In [24]: pd.unique(df['A']).tolist()
    Out[24]: [1, 2, 3]

Here is a more complex example. Say we want to find the unique values from column 'B' where 'A' is equal to 1. 

First, let's introduce a duplicate so you can see how it works. Let's replace the 6 in row '4', column 'B' with a 4:

    In [24]: df.loc['4', 'B'] = 4    
    Out[24]:    
       A  B
    0  1  5
    1  1  4
    2  2  3
    3  3  4
    4  1  4
    5  1  7

Now select the data: 

    In [25]: pd.unique(df[df['A'] == 1 ]['B']).tolist()
    Out[25]: [5, 4, 7]

This can be broken down by thinking of the inner DataFrame first: 

    df['A'] == 1 

This finds values in column A that are equal to 1, and applies True or False to them. We can then use this to select values from column 'B' of the DataFrame (the outer DataFrame selection)

For comparison, here is the list if we don't use unique. It retrieves every value in column 'B' where column 'A' is 1

    In [26]: df[df['A'] == 1]['B'].tolist()
    Out[26]: [5, 4, 4, 7]


