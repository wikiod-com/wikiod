---
title: "MultiIndex"
slug: "multiindex"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Iterate over DataFrame with MultiIndex
Given the following DataFrame:

    In [11]: df = pd.DataFrame({'a':[1,1,1,2,2,3],'b':[4,4,5,5,6,7,],'c':[10,11,12,13,14,15]})

    In [12]: df.set_index(['a','b'], inplace=True)

    In [13]: df
    Out[13]: 
          c
    a b    
    1 4  10
      4  11
      5  12
    2 5  13
      6  14
    3 7  15

You can iterate by any level of the MultiIndex. For example, `level=0` (you can also select the level by name e.g. `level='a'`):

    In[21]: for idx, data in df.groupby(level=0):
                print('---')
                print(data)
    ---
          c
    a b    
    1 4  10
      4  11
      5  12
    ---
          c
    a b    
    2 5  13
      6  14
    ---
          c
    a b    
    3 7  15

You can also select the levels by name e.g. `level='b':

    In[22]: for idx, data in df.groupby(level='b'):
                print('---')
                print(data)
    ---     
          c
    a b    
    1 4  10
      4  11
    ---
          c
    a b    
    1 5  12
    2 5  13
    ---
          c
    a b    
    2 6  14
    ---
          c
    a b    
    3 7  15


## Select from MultiIndex by Level
Given the following DataFrame:

    In [11]: df = pd.DataFrame(np.random.randn(6, 3), columns=['A', 'B', 'C'])

    In [12]: df.set_index(['A', 'B'], inplace=True)

    In [13]: df
    Out[13]: 
                                C
    A         B                  
     0.902764 -0.259656 -1.864541
    -0.695893  0.308893  0.125199
     1.696989 -1.221131 -2.975839
    -1.132069 -1.086189 -1.945467
     2.294835 -1.765507  1.567853
    -1.788299  2.579029  0.792919

Get the values of `A`, by name:

    In [14]: df.index.get_level_values('A')
    Out[14]: 
    Float64Index([0.902764041011, -0.69589264969,  1.69698924476, -1.13206872067,
                   2.29483481146,   -1.788298829],
                 dtype='float64', name='A')
Or by number of level:

    In [15]: df.index.get_level_values(level=0)
    Out[15]: 
    Float64Index([0.902764041011, -0.69589264969,  1.69698924476, -1.13206872067,
                   2.29483481146,   -1.788298829],
                 dtype='float64', name='A')

And for a specific range:

    In [16]: df.loc[(df.index.get_level_values('A') > 0.5) & (df.index.get_level_values('A') < 2.1)]
    Out[16]:
                               C
    A        B                  
    0.902764 -0.259656 -1.864541
    1.696989 -1.221131 -2.975839
Range can also include multiple columns:

    In [17]: df.loc[(df.index.get_level_values('A') > 0.5) & (df.index.get_level_values('B') < 0)]
    Out[17]: 
                               C
    A        B                  
    0.902764 -0.259656 -1.864541
    1.696989 -1.221131 -2.975839
    2.294835 -1.765507  1.567853

To extract a specific value you can use xs (cross-section):

    In [18]: df.xs(key=0.9027639999999999)
    Out[18]:
                      C
    B
    -0.259656 -1.864541
    
    In [19]: df.xs(key=0.9027639999999999, drop_level=False)
    Out[19]:
                               C
    A        B
    0.902764 -0.259656 -1.864541

## Setting and sorting a MultiIndex
This example shows how to use column data to set a `MultiIndex` in a `pandas.DataFrame`.   

    In [1]: df = pd.DataFrame([['one', 'A', 100], ['two', 'A', 101], ['three', 'A', 102],
       ...:                    ['one', 'B', 103], ['two', 'B', 104], ['three', 'B', 105]],
       ...:                   columns=['c1', 'c2', 'c3'])


    In [2]: df
    Out[2]: 
          c1 c2   c3
    0    one  A  100
    1    two  A  101
    2  three  A  102
    3    one  B  103
    4    two  B  104
    5  three  B  105


    
    In [3]: df.set_index(['c1', 'c2'])
    Out[3]: 
               c3
    c1    c2     
    one   A   100
    two   A   101
    three A   102
    one   B   103
    two   B   104
    three B   105

You can sort the index right after you set it:    

    In [4]: df.set_index(['c1', 'c2']).sort_index()
    Out[4]: 
               c3
    c1    c2     
    one   A   100
          B   103
    three A   102
          B   105
    two   A   101
          B   104


Having a sorted index, will result in slightly more efficient lookups on the first level:

    In [5]: df_01 = df.set_index(['c1', 'c2'])
    
    In [6]: %timeit df_01.loc['one']
    1000 loops, best of 3: 607 µs per loop
    

    In [7]: df_02 = df.set_index(['c1', 'c2']).sort_index()
    
    In [8]: %timeit df_02.loc['one']
    1000 loops, best of 3: 413 µs per loop


After the index has been set, you can perform lookups for specific records or groups of records:

    In [9]: df_indexed = df.set_index(['c1', 'c2']).sort_index()
    
    In [10]: df_indexed.loc['one']
    Out[10]: 
         c3
    c2     
    A   100
    B   103
    

    In [11]: df_indexed.loc['one', 'A']
    Out[11]: 
    c3    100
    Name: (one, A), dtype: int64
    

    In [12]: df_indexed.xs((slice(None), 'A'))
    Out[12]: 
            c3
    c1        
    one    100
    three  102
    two    101



## How to change MultiIndex columns to standard columns
Given a DataFrame with MultiIndex columns

    # build an example DataFrame
    midx = pd.MultiIndex(levels=[['zero', 'one'], ['x','y']], labels=[[1,1,0,],[1,0,1,]])
    df = pd.DataFrame(np.random.randn(2,3), columns=midx)

    In [2]: df
    Out[2]: 
            one                zero
              y         x         y
    0  0.785806 -0.679039  0.513451
    1 -0.337862 -0.350690 -1.423253

If you want to change the columns to standard columns (not MultiIndex), just rename the columns.  

    df.columns = ['A','B','C']
    In [3]: df
    Out[3]: 
              A         B         C
    0  0.785806 -0.679039  0.513451
    1 -0.337862 -0.350690 -1.423253


## How to change standard columns to MultiIndex
Start with a standard DataFrame

    df = pd.DataFrame(np.random.randn(2,3), columns=['a','b','c'])

    In [91]: df
    Out[91]: 
              a         b         c
    0 -0.911752 -1.405419 -0.978419
    1  0.603888 -1.187064 -0.035883

Now to change to MultiIndex, create a `MultiIndex` object and assign it to `df.columns`.

    midx = pd.MultiIndex(levels=[['zero', 'one'], ['x','y']], labels=[[1,1,0,],[1,0,1,]])
    df.columns = midx

    In [94]: df
    Out[94]: 
                one                zero
                  y         x         y
        0 -0.911752 -1.405419 -0.978419
        1  0.603888 -1.187064 -0.035883

## MultiIndex Columns
MultiIndex can also be used to create DataFrames with multilevel columns. Just use the `columns` keyword in the DataFrame command.

    midx = pd.MultiIndex(levels=[['zero', 'one'], ['x','y']], labels=[[1,1,0,],[1,0,1,]])
    df = pd.DataFrame(np.random.randn(6,4), columns=midx)

    In [86]: df
    Out[86]: 
            one                zero
              y         x         y
    0  0.625695  2.149377  0.006123
    1 -1.392909  0.849853  0.005477

## Displaying all elements in the index
To view all elements in the index change the print options that “sparsifies” the display of the MultiIndex.
```
pd.set_option('display.multi_sparse', False)
df.groupby(['A','B']).mean()
# Output:
#        C
# A B
# a 1  107
# a 2  102
# a 3  115
# b 5   92
# b 8   98
# c 2   87
# c 4  104
# c 9  123
```

