---
title: "Creating DataFrames"
slug: "creating-dataframes"
draft: false
images: []
weight: 9734
type: docs
toc: true
---

**DataFrame** is a data structure provided by pandas library,apart from *Series* & *Panel*. It is a 2-dimensional structure & can be compared to a  table of rows and columns. 

Each row can be identified by an integer index (0..N) or a label explicitly set when creating a DataFrame object.  Each column can be of distinct type and is identified by a label.

This topic covers various ways to construct/create a DataFrame object. Ex. from Numpy arrays, from list of tuples, from dictionary.

## Create a sample DataFrame
    import pandas as pd

Create a DataFrame from a dictionary, containing two columns: `numbers` and `colors`. Each key represent a column name and the value is a series of data, the content of the column:
    
    df = pd.DataFrame({'numbers': [1, 2, 3], 'colors': ['red', 'white', 'blue']})
  
Show contents of dataframe:
  
    print(df)
    # Output: 
    #   colors  numbers
    # 0    red        1
    # 1  white        2
    # 2   blue        3

Pandas orders columns alphabetically as `dict` are not ordered. To specify the order, use the `columns` parameter.

    df = pd.DataFrame({'numbers': [1, 2, 3], 'colors': ['red', 'white', 'blue']}, 
                      columns=['numbers', 'colors'])

    print(df)  
    # Output:     
    #    numbers colors
    # 0        1    red
    # 1        2  white
    # 2        3   blue

## Create a sample DataFrame using Numpy
Create a `DataFrame` of random numbers:

    import numpy as np
    import pandas as pd
    
    # Set the seed for a reproducible sample
    np.random.seed(0)  
    
    df = pd.DataFrame(np.random.randn(5, 3), columns=list('ABC'))

    print(df)
    # Output:
    #           A         B         C
    # 0  1.764052  0.400157  0.978738
    # 1  2.240893  1.867558 -0.977278
    # 2  0.950088 -0.151357 -0.103219
    # 3  0.410599  0.144044  1.454274
    # 4  0.761038  0.121675  0.443863

Create a `DataFrame` with integers:

    df = pd.DataFrame(np.arange(15).reshape(5,3),columns=list('ABC'))

    print(df)
    # Output:
    #     A   B   C
    # 0   0   1   2
    # 1   3   4   5
    # 2   6   7   8
    # 3   9  10  11
    # 4  12  13  14

Create a `DataFrame` and include nans (`NaT, NaN, 'nan', None`) across columns and rows:

    df = pd.DataFrame(np.arange(48).reshape(8,6),columns=list('ABCDEF'))

    print(df)
    # Output: 
    #     A   B   C   D   E   F
    # 0   0   1   2   3   4   5
    # 1   6   7   8   9  10  11
    # 2  12  13  14  15  16  17
    # 3  18  19  20  21  22  23
    # 4  24  25  26  27  28  29
    # 5  30  31  32  33  34  35
    # 6  36  37  38  39  40  41
    # 7  42  43  44  45  46  47

    df.ix[::2,0] = np.nan # in column 0, set elements with indices 0,2,4, ... to NaN 
    df.ix[::4,1] = pd.NaT # in column 1, set elements with indices 0,4, ... to np.NaT
    df.ix[:3,2] = 'nan'   # in column 2, set elements with index from 0 to 3 to 'nan'
    df.ix[:,5] = None     # in column 5, set all elements to None
    df.ix[5,:] = None     # in row 5, set all elements to None    
    df.ix[7,:] = np.nan   # in row 7, set all elements to NaN
    
    print(df)
    # Output:
    #     A     B     C   D   E     F
    # 0 NaN   NaT   nan   3   4  None
    # 1   6     7   nan   9  10  None
    # 2 NaN    13   nan  15  16  None
    # 3  18    19   nan  21  22  None
    # 4 NaN   NaT    26  27  28  None
    # 5 NaN  None  None NaN NaN  None
    # 6 NaN    37    38  39  40  None
    # 7 NaN   NaN   NaN NaN NaN   NaN



## Create a sample DataFrame with datetime
    import pandas as pd
    import numpy as np
    
    np.random.seed(0)
    # create an array of 5 dates starting at '2015-02-24', one per minute
    rng = pd.date_range('2015-02-24', periods=5, freq='T')
    df = pd.DataFrame({ 'Date': rng, 'Val': np.random.randn(len(rng)) }) 
 
    print (df)
    # Output:
    #                  Date       Val
    # 0 2015-02-24 00:00:00  1.764052
    # 1 2015-02-24 00:01:00  0.400157
    # 2 2015-02-24 00:02:00  0.978738
    # 3 2015-02-24 00:03:00  2.240893
    # 4 2015-02-24 00:04:00  1.867558
    
    # create an array of 5 dates starting at '2015-02-24', one per day
    rng = pd.date_range('2015-02-24', periods=5, freq='D')
    df = pd.DataFrame({ 'Date': rng, 'Val' : np.random.randn(len(rng))}) 
 
    print (df)
    # Output:
    #         Date       Val
    # 0 2015-02-24 -0.977278
    # 1 2015-02-25  0.950088
    # 2 2015-02-26 -0.151357
    # 3 2015-02-27 -0.103219
    # 4 2015-02-28  0.410599
    
    # create an array of 5 dates starting at '2015-02-24', one every 3 years
    rng = pd.date_range('2015-02-24', periods=5, freq='3A')
    df = pd.DataFrame({ 'Date': rng, 'Val' : np.random.randn(len(rng))})  

    print (df)
    # Output:
    #         Date       Val
    # 0 2015-12-31  0.144044
    # 1 2018-12-31  1.454274
    # 2 2021-12-31  0.761038
    # 3 2024-12-31  0.121675
    # 4 2027-12-31  0.443863

**DataFrame with [`DatetimeIndex`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DatetimeIndex.html)**:

    import pandas as pd
    import numpy as np
    
    np.random.seed(0)
    rng = pd.date_range('2015-02-24', periods=5, freq='T')
    df = pd.DataFrame({ 'Val' : np.random.randn(len(rng)) }, index=rng)  

    print (df)
    # Output:
    #                           Val
    # 2015-02-24 00:00:00  1.764052
    # 2015-02-24 00:01:00  0.400157
    # 2015-02-24 00:02:00  0.978738
    # 2015-02-24 00:03:00  2.240893
    # 2015-02-24 00:04:00  1.867558


[`Offset-aliases`](http://pandas.pydata.org/pandas-docs/stable/timeseries.html#offset-aliases) for parameter `freq` in [`date_range`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.date_range.html):

    Alias     Description
    B         business day frequency  
    C         custom business day frequency (experimental)  
    D         calendar day frequency  
    W         weekly frequency  
    M         month end frequency  
    BM        business month end frequency  
    CBM       custom business month end frequency  
    MS        month start frequency  
    BMS       business month start frequency  
    CBMS      custom business month start frequency  
    Q         quarter end frequency  
    BQ        business quarter endfrequency  
    QS        quarter start frequency  
    BQS       business quarter start frequency  
    A         year end frequency  
    BA        business year end frequency  
    AS        year start frequency  
    BAS       business year start frequency  
    BH        business hour frequency  
    H         hourly frequency  
    T, min    minutely frequency  
    S         secondly frequency  
    L, ms     milliseconds  
    U, us     microseconds  
    N         nanoseconds  

## Create a sample DataFrame with MultiIndex
    import pandas as pd
    import numpy as np

**Using [`from_tuples`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.MultiIndex.from_tuples.html):**
    
    np.random.seed(0)
    tuples = list(zip(*[['bar', 'bar', 'baz', 'baz',
                         'foo', 'foo', 'qux', 'qux'],
                          ['one', 'two', 'one', 'two',
                           'one', 'two', 'one', 'two']]))
    
    idx = pd.MultiIndex.from_tuples(tuples, names=['first', 'second'])

**Using [`from_product`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.MultiIndex.from_product.html):**

    idx = pd.MultiIndex.from_product([['bar', 'baz', 'foo', 'qux'],['one','two']])

Then, use this MultiIndex:

    df = pd.DataFrame(np.random.randn(8, 2), index=idx, columns=['A', 'B'])
    print (df)
                         A         B
    first second                    
    bar   one     1.764052  0.400157
          two     0.978738  2.240893
    baz   one     1.867558 -0.977278
          two     0.950088 -0.151357
    foo   one    -0.103219  0.410599
          two     0.144044  1.454274
    qux   one     0.761038  0.121675
          two     0.443863  0.333674

## Create a DataFrame from a list of tuples
You can create a DataFrame from a list of simple tuples, and can even choose the specific elements of the tuples you want to use. Here we will create a DataFrame using all of the data in each tuple except for the last element.

    import pandas as pd
    
    data = [
    ('p1', 't1', 1, 2),
    ('p1', 't2', 3, 4),
    ('p2', 't1', 5, 6),
    ('p2', 't2', 7, 8),
    ('p2', 't3', 2, 8)
    ]
    
    df = pd.DataFrame(data)
    
    print(df)
    #     0   1  2  3
    # 0  p1  t1  1  2
    # 1  p1  t2  3  4
    # 2  p2  t1  5  6
    # 3  p2  t2  7  8
    # 4  p2  t3  2  8

   



## Create a DataFrame from a list of dictionaries
A DataFrame can be created from a list of dictionaries. Keys are used as column names.

```
import pandas as pd
L = [{'Name': 'John', 'Last Name': 'Smith'}, 
         {'Name': 'Mary', 'Last Name': 'Wood'}]
pd.DataFrame(L)
# Output:  Last Name  Name
# 0     Smith  John
# 1      Wood  Mary
```

Missing values are filled with `NaN`s 

```
L = [{'Name': 'John', 'Last Name': 'Smith', 'Age': 37},
     {'Name': 'Mary', 'Last Name': 'Wood'}]
pd.DataFrame(L)
# Output:     Age Last Name  Name
#          0   37     Smith  John
#          1  NaN      Wood  Mary
```


## Create a sample DataFrame from multiple collections using Dictionary
```python
import pandas as pd
import numpy as np

np.random.seed(123) 
x = np.random.standard_normal(4)
y = range(4)
df = pd.DataFrame({'X':x, 'Y':y})
>>> df
          X  Y
0 -1.085631  0
1  0.997345  1
2  0.282978  2
3 -1.506295  3
```

## Create a DataFrame from a dictionary of lists
Create a DataFrame from multiple lists by passing a dict whose values lists. The keys of the dictionary are used as column labels. The lists can also be ndarrays. The lists/ndarrays must all be the same length.
```
import pandas as pd
    
# Create DF from dict of lists/ndarrays
df = pd.DataFrame({'A' : [1, 2, 3, 4],
                       'B' : [4, 3, 2, 1]})
df
# Output:
#       A  B
#    0  1  4
#    1  2  3
#    2  3  2
#    3  4  1
```

If the arrays are not the same length an error is raised

```
df = pd.DataFrame({'A' : [1, 2, 3, 4], 'B' : [5, 5, 5]}) # a ValueError is raised
```

Using ndarrays 

```
import pandas as pd
import numpy as np

np.random.seed(123) 
x = np.random.standard_normal(4)
y = range(4)
df = pd.DataFrame({'X':x, 'Y':y})
df
# Output:           X  Y
#         0 -1.085631  0
#         1  0.997345  1
#         2  0.282978  2
#         3 -1.506295  3
```
See additional details at: http://pandas.pydata.org/pandas-docs/stable/dsintro.html#from-dict-of-ndarrays-lists

## Save and Load a DataFrame in pickle (.plk) format
    import pandas as pd

    # Save dataframe to pickled pandas object
    df.to_pickle(file_name) # where to save it usually as a .plk
    
    # Load dataframe from pickled pandas object
    df= pd.read_pickle(file_name)

