---
title: "Grouping Data"
slug: "grouping-data"
draft: false
images: []
weight: 9891
type: docs
toc: true
---

## Aggregating by size versus by count
The difference between `size` and `count` is:

[`size`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.core.groupby.GroupBy.size.html#pandas.core.groupby.GroupBy.size) counts `NaN` values, [`count`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.core.groupby.GroupBy.count.html#pandas.core.groupby.GroupBy.count) does not.

    df = pd.DataFrame(
            {"Name":["Alice", "Bob", "Mallory", "Mallory", "Bob" , "Mallory"],
             "City":["Seattle", "Seattle", "Portland", "Seattle", "Seattle", "Portland"],
             "Val": [4, 3, 3, np.nan, np.nan, 4]})
    
    df
    # Output: 
    #        City     Name  Val
    # 0   Seattle    Alice  4.0
    # 1   Seattle      Bob  3.0
    # 2  Portland  Mallory  3.0
    # 3   Seattle  Mallory  NaN
    # 4   Seattle      Bob  NaN
    # 5  Portland  Mallory  4.0
    
    
    df.groupby(["Name", "City"])['Val'].size().reset_index(name='Size')
    # Output: 
    #       Name      City  Size
    # 0    Alice   Seattle     1
    # 1      Bob   Seattle     2
    # 2  Mallory  Portland     2
    # 3  Mallory   Seattle     1

    df.groupby(["Name", "City"])['Val'].count().reset_index(name='Count')
    # Output: 
    #       Name      City  Count
    # 0    Alice   Seattle      1
    # 1      Bob   Seattle      1
    # 2  Mallory  Portland      2
    # 3  Mallory   Seattle      0


## Basic grouping
### Group by one column

Using the following DataFrame

    df = pd.DataFrame({'A': ['a', 'b', 'c', 'a', 'b', 'b'], 
                       'B': [2, 8, 1, 4, 3, 8], 
                       'C': [102, 98, 107, 104, 115, 87]})
    
    df
    # Output: 
    #    A  B    C
    # 0  a  2  102
    # 1  b  8   98
    # 2  c  1  107
    # 3  a  4  104
    # 4  b  3  115
    # 5  b  8   87


Group by column A and get the mean value of other columns:

    df.groupby('A').mean()
    # Output: 
    #           B    C
    # A               
    # a  3.000000  103
    # b  6.333333  100
    # c  1.000000  107


### Group by multiple columns

    df.groupby(['A','B']).mean()
    # Output: 
    #          C
    # A B       
    # a 2  102.0
    #   4  104.0
    # b 3  115.0
    #   8   92.5
    # c 1  107.0

Note how after grouping each row in the resulting DataFrame is indexed by a tuple or [MultiIndex](https://www.wikiod.com/pandas/multiindex) (in this case a pair of elements from columns A and B).

To apply several aggregation methods at once, for instance to count the number of items in each group and compute their mean, use the `agg` function:


    df.groupby(['A','B']).agg(['count', 'mean'])
    # Output:
    #         C       
    #     count   mean
    # A B             
    # a 2     1  102.0
    #   4     1  104.0
    # b 3     1  115.0
    #   8     2   92.5
    # c 1     1  107.0
 


## Column selection of a group
When you do a groupby you can select either a single column or a list of columns:

```python
In [11]: df = pd.DataFrame([[1, 1, 2], [1, 2, 3], [2, 3, 4]], columns=["A", "B", "C"])

In [12]: df
Out[12]:
   A  B  C
0  1  1  2
1  1  2  3
2  2  3  4

In [13]: g = df.groupby("A")

In [14]: g["B"].mean()           # just column B
Out[14]:
A
1    1.5
2    3.0
Name: B, dtype: float64

In [15]: g[["B", "C"]].mean()    # columns B and C
Out[15]:
     B    C
A
1  1.5  2.5
2  3.0  4.0
```

You can also use `agg` to specify columns and aggregation to perform:

```python
In [16]: g.agg({'B': 'mean', 'C': 'count'})
Out[16]:
   C    B
A        
1  2  1.5
2  1  3.0
```


## using transform to get group-level statistics while preserving the original dataframe
example:

    df = pd.DataFrame({'group1' :  ['A', 'A', 'A', 'A',
                                   'B', 'B', 'B', 'B'],
                       'group2' :  ['C', 'C', 'C', 'D',
                                   'E', 'E', 'F', 'F'],
                       'B'      :  ['one', np.NaN, np.NaN, np.NaN,
                                    np.NaN, 'two', np.NaN, np.NaN],
                       'C'      :  [np.NaN, 1, np.NaN, np.NaN,
                                   np.NaN, np.NaN, np.NaN, 4]})           

     df
    Out[34]: 
         B    C group1 group2
    0  one  NaN      A      C
    1  NaN  1.0      A      C
    2  NaN  NaN      A      C
    3  NaN  NaN      A      D
    4  NaN  NaN      B      E
    5  two  NaN      B      E
    6  NaN  NaN      B      F
    7  NaN  4.0      B      F
                        
I want to get the count of non-missing observations of B for each combination of `group1` and `group2`. `groupby.transform` is a very powerful function that does exactly that.

    df['count_B']=df.groupby(['group1','group2']).B.transform('count')                        
    
    df
    Out[36]: 
         B    C group1 group2  count_B
    0  one  NaN      A      C        1
    1  NaN  1.0      A      C        1
    2  NaN  NaN      A      C        1
    3  NaN  NaN      A      D        0
    4  NaN  NaN      B      E        1
    5  two  NaN      B      E        1
    6  NaN  NaN      B      F        0
    7  NaN  4.0      B      F        0

## Grouping numbers
For the following DataFrame:

    import numpy as np
    import pandas as pd
    np.random.seed(0)
    df = pd.DataFrame({'Age': np.random.randint(20, 70, 100), 
                       'Sex': np.random.choice(['Male', 'Female'], 100), 
                       'number_of_foo': np.random.randint(1, 20, 100)})
    df.head()
    # Output: 

    #    Age     Sex  number_of_foo
    # 0   64  Female             14
    # 1   67  Female             14
    # 2   20  Female             12
    # 3   23    Male             17
    # 4   23  Female             15

Group `Age` into three categories (or bins). 
Bins can be given as 

 - an integer `n` indicating the number of bins—in this case the dataframe's data is divided into `n` intervals of equal size
 - a sequence of integers denoting the endpoint of the left-open intervals in which the data is divided into—for instance `bins=[19, 40, 65, np.inf]` creates three age groups `(19, 40]`, `(40, 65]`, and `(65, np.inf]`.  

Pandas assigns automatically the string versions of the intervals as label. It is also possible to define own labels by defining a `labels` parameter as a list of strings.

    pd.cut(df['Age'], bins=4)
    # this creates four age groups: (19.951, 32.25] < (32.25, 44.5] < (44.5, 56.75] < (56.75, 69]
    Name: Age, dtype: category
    Categories (4, object): [(19.951, 32.25] < (32.25, 44.5] < (44.5, 56.75] < (56.75, 69]]

    pd.cut(df['Age'], bins=[19, 40, 65, np.inf])
    # this creates three age groups: (19, 40], (40, 65] and (65, infinity)
    Name: Age, dtype: category
    Categories (3, object): [(19, 40] < (40, 65] < (65, inf]]
    

Use it in `groupby` to get the mean number of foo:

    age_groups = pd.cut(df['Age'], bins=[19, 40, 65, np.inf])
    df.groupby(age_groups)['number_of_foo'].mean()
    # Output: 
    # Age
    # (19, 40]     9.880000
    # (40, 65]     9.452381
    # (65, inf]    9.250000
    # Name: number_of_foo, dtype: float64

Cross tabulate age groups and gender:

    pd.crosstab(age_groups, df['Sex'])
    # Output: 
    # Sex        Female  Male
    # Age
    # (19, 40]       22    28
    # (40, 65]       18    24
    # (65, inf]       3     5

## Aggregating groups

    In [1]: import numpy as np   
    In [2]: import pandas as pd
   
    In [3]: df = pd.DataFrame({'A': list('XYZXYZXYZX'), 'B': [1, 2, 1, 3, 1, 2, 3, 3, 1, 2], 
                               'C': [12, 14, 11, 12, 13, 14, 16, 12, 10, 19]})
    
    In [4]: df.groupby('A')['B'].agg({'mean': np.mean, 'standard deviation': np.std})
    Out[4]: 
       standard deviation      mean
    A                              
    X            0.957427  2.250000
    Y            1.000000  2.000000
    Z            0.577350  1.333333

For multiple columns:

    In [5]: df.groupby('A').agg({'B': [np.mean, np.std], 'C': [np.sum, 'count']})
    Out[5]: 
        C               B          
      sum count      mean       std
    A                              
    X  59     4  2.250000  0.957427
    Y  39     3  2.000000  1.000000
    Z  35     3  1.333333  0.577350

## Export groups in different files
You can iterate on the object returned by `groupby()`. The iterator contains `(Category, DataFrame)` tuples. 

    # Same example data as in the previous example.
    import numpy as np
    import pandas as pd
    np.random.seed(0)
    df = pd.DataFrame({'Age': np.random.randint(20, 70, 100), 
                       'Sex': np.random.choice(['Male', factor'Female'], 100), 
                       'number_of_foo': np.random.randint(1, 20, 100)})

    # Export to Male.csv and Female.csv files.
    for sex, data in df.groupby('Sex'):
        data.to_csv("{}.csv".format(sex))


