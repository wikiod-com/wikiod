---
title: "Data Types"
slug: "data-types"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

dtypes are not native to pandas. They are a result of pandas close architectural coupling to numpy.

the dtype of a column does not in any way have to correlate to the python type of the object contained in the column.

Here we have a `pd.Series` with floats. The dtype will be `float`.

Then we use `astype` to "cast" it to object. 

    pd.Series([1.,2.,3.,4.,5.]).astype(object)
    0    1
    1    2
    2    3
    3    4
    4    5
    dtype: object

The dtype is now object, but the objects in the list are still float. Logical if you know that in python, everything is an object, and can be upcasted to object.

    type(pd.Series([1.,2.,3.,4.,5.]).astype(object)[0])
    float

Here we try "casting" the floats to strings.

    pd.Series([1.,2.,3.,4.,5.]).astype(str)
    0    1.0
    1    2.0
    2    3.0
    3    4.0
    4    5.0
    dtype: object

The dtype is now object, but the type of the entries in the list are string.
This is because `numpy` does not deal with strings, and thus acts as if they are just objects and of no concern.
    
    type(pd.Series([1.,2.,3.,4.,5.]).astype(str)[0])
    str
    
Do not trust dtypes, they are an artifact of an architectural flaw in pandas. Specify them as you must, but do not rely on what dtype is set on a column.

## Changing dtypes
`astype()` method changes the dtype of a Series and returns a new Series.

    In [1]: df = pd.DataFrame({'A': [1, 2, 3], 'B': [1.0, 2.0, 3.0], 
                               'C': ['1.1.2010', '2.1.2011', '3.1.2011'], 
                               'D': ['1 days', '2 days', '3 days'],
                               'E': ['1', '2', '3']})
    In [2]: df
    Out[2]:
       A    B          C       D  E
    0  1  1.0   1.1.2010  1 days  1
    1  2  2.0   2.1.2011  2 days  2
    2  3  3.0   3.1.2011  3 days  3

    In [3]: df.dtypes
    Out[3]:
    A      int64
    B    float64
    C     object
    D     object
    E     object
    dtype: object

Change the type of column A to float, and type of column B to integer:

    In [4]: df['A'].astype('float')
    Out[4]:
    0    1.0
    1    2.0
    2    3.0
    Name: A, dtype: float64

    In [5]: df['B'].astype('int')
    Out[5]:
    0    1
    1    2
    2    3
    Name: B, dtype: int32

`astype()` method is for specific type conversion (i.e. you can specify `.astype(float64')`, `.astype(float32)`, or `.astype(float16)`). For general conversion, you can use `pd.to_numeric`, `pd.to_datetime` and `pd.to_timedelta`.

## Changing the type to numeric ##

`pd.to_numeric` changes the values to a numeric type.

    In [6]: pd.to_numeric(df['E'])
    Out[6]:
    0    1
    1    2
    2    3
    Name: E, dtype: int64

By default, `pd.to_numeric` raises an error if an input cannot be converted to a number. You can change that behavior by using the `errors` parameter.

    # Ignore the error, return the original input if it cannot be converted
    In [7]: pd.to_numeric(pd.Series(['1', '2', 'a']), errors='ignore')
    Out[7]:
    0    1
    1    2
    2    a
    dtype: object

    # Return NaN when the input cannot be converted to a number
    In [8]: pd.to_numeric(pd.Series(['1', '2', 'a']), errors='coerce')
    Out[8]:
    0    1.0
    1    2.0
    2    NaN
    dtype: float64

If need check all rows with input cannot be converted to numeric use [`boolean indexing`](http://pandas.pydata.org/pandas-docs/stable/indexing.html#boolean-indexing) with [`isnull`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.isnull.html):

    In [9]: df = pd.DataFrame({'A': [1, 'x', 'z'],
                               'B': [1.0, 2.0, 3.0],
                               'C': [True, False, True]})
    
    In [10]: pd.to_numeric(df.A, errors='coerce').isnull()
    Out[10]: 
    0    False
    1     True
    2     True
    Name: A, dtype: bool
    
    In [11]: df[pd.to_numeric(df.A, errors='coerce').isnull()]
    Out[11]: 
       A    B      C
    1  x  2.0  False
    2  z  3.0   True

## Changing the type to datetime ##

    In [12]: pd.to_datetime(df['C'])
    Out[12]:
    0   2010-01-01
    1   2011-02-01
    2   2011-03-01
    Name: C, dtype: datetime64[ns]

Note that 2.1.2011 is converted to February 1, 2011. If you want January 2, 2011 instead, you need to use the `dayfirst` parameter.

    In [13]: pd.to_datetime('2.1.2011', dayfirst=True)
    Out[13]: Timestamp('2011-01-02 00:00:00')

## Changing the type to timedelta ##

    In [14]: pd.to_timedelta(df['D'])
    Out[14]:
    0   1 days
    1   2 days
    2   3 days
    Name: D, dtype: timedelta64[ns]


## Selecting columns based on dtype
`select_dtypes` method can be used to select columns based on dtype.

    In [1]: df = pd.DataFrame({'A': [1, 2, 3], 'B': [1.0, 2.0, 3.0], 'C': ['a', 'b', 'c'], 
                               'D': [True, False, True]})
    
    In [2]: df
    Out[2]: 
       A    B  C      D
    0  1  1.0  a   True
    1  2  2.0  b  False
    2  3  3.0  c   True
    
With `include` and `exclude` parameters you can specify which types you want:

    # Select numbers
    In [3]: df.select_dtypes(include=['number'])  # You need to use a list
    Out[3]:
       A    B
    0  1  1.0
    1  2  2.0
    2  3  3.0    
    
    # Select numbers and booleans
    In [4]: df.select_dtypes(include=['number', 'bool'])
    Out[4]:
       A    B      D
    0  1  1.0   True
    1  2  2.0  False
    2  3  3.0   True
    
    # Select numbers and booleans but exclude int64
    In [5]: df.select_dtypes(include=['number', 'bool'], exclude=['int64'])
    Out[5]:
         B      D
    0  1.0   True
    1  2.0  False
    2  3.0   True
    

## Checking the types of columns
Types of columns can be checked by `.dtypes` atrribute of DataFrames.

    In [1]: df = pd.DataFrame({'A': [1, 2, 3], 'B': [1.0, 2.0, 3.0], 'C': [True, False, True]})

    In [2]: df
    Out[2]:
       A    B      C
    0  1  1.0   True
    1  2  2.0  False
    2  3  3.0   True

    In [3]: df.dtypes
    Out[3]:
    A      int64
    B    float64
    C       bool
    dtype: object

For a single series, you can use `.dtype` attribute.

    In [4]: df['A'].dtype
    Out[4]: dtype('int64')

## Summarizing dtypes
`get_dtype_counts` method can be used to see a breakdown of dtypes.

    In [1]: df = pd.DataFrame({'A': [1, 2, 3], 'B': [1.0, 2.0, 3.0], 'C': ['a', 'b', 'c'], 
                               'D': [True, False, True]})
    
    In [2]: df.get_dtype_counts()
    Out[2]: 
    bool       1
    float64    1
    int64      1
    object     1
    dtype: int64


