---
title: "Series"
slug: "series"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Series with datetime
    import pandas as pd
    import numpy as np
    
    np.random.seed(0)
    rng = pd.date_range('2015-02-24', periods=5, freq='T')
    s = pd.Series(np.random.randn(len(rng)), index=rng)  
    print (s)
    
    2015-02-24 00:00:00    1.764052
    2015-02-24 00:01:00    0.400157
    2015-02-24 00:02:00    0.978738
    2015-02-24 00:03:00    2.240893
    2015-02-24 00:04:00    1.867558
    Freq: T, dtype: float64
    
    rng = pd.date_range('2015-02-24', periods=5, freq='T')
    s1 = pd.Series(rng)  
    print (s1)
    
    0   2015-02-24 00:00:00
    1   2015-02-24 00:01:00
    2   2015-02-24 00:02:00
    3   2015-02-24 00:03:00
    4   2015-02-24 00:04:00
    dtype: datetime64[ns]


## Simple Series creation examples
A series is a one-dimension data structure. It's a bit like a supercharged array, or a dictionary. 

    import pandas as pd
    
    s = pd.Series([10, 20, 30])
    
    >>> s
    0    10
    1    20
    2    30
    dtype: int64
    
Every value in a series has an index. By default, the indices are integers, running from 0 to the series length minus 1. In the example above you can see the indices printed to the left of the values. 

You can specify your own indices:

    s2 = pd.Series([1.5, 2.5, 3.5], index=['a', 'b', 'c'], name='my_series')
    
    >>> s2
    a    1.5
    b    2.5
    c    3.5
    Name: my_series, dtype: float64
    
    s3 = pd.Series(['a', 'b', 'c'], index=list('ABC'))

    >>> s3
    A    a
    B    b
    C    c
    dtype: object
    

## A few quick tips about Series in Pandas
Let us assume we have the following Series:

    >>> import pandas as pd
    >>> s = pd.Series([1, 4, 6, 3, 8, 7, 4, 5])
    >>> s
    0    1
    1    4
    2    6
    3    3
    4    8
    5    7
    6    4
    7    5
    dtype: int64

Followings are a few simple things which come handy when you are working with Series:

To get the length of s:

    >>> len(s)
    8

To access an element in s:

    >>> s[4]
    8

To access an element in s using the index:

    >>> s.loc[2]
    6

To access a sub-Series inside s:

    >>> s[1:3]
    1    4
    2    6
    dtype: int64

To get a sub-Series of s with values larger than 5:

    >>> s[s > 5]
    2    6
    4    8
    5    7
    dtype: int64

To get the minimum, maximum, mean, and standard deviation:

    >>> s.min()
    1
    >>> s.max()
    8
    >>> s.mean()
    4.75
    >>> s.std()
    2.2519832529192065

To convert the Series type to float:

    >>> s.astype(float)
    0    1.0
    1    4.0
    2    6.0
    3    3.0
    4    8.0
    5    7.0
    6    4.0
    7    5.0
    dtype: float64

To get the values in s as a numpy array:

    >>> s.values
    array([1, 4, 6, 3, 8, 7, 4, 5])

To make a copy of s:

    >>> d = s.copy()
    >>> d
    0    1
    1    4
    2    6
    3    3
    4    8
    5    7
    6    4
    7    5
    dtype: int64




## Applying a function to a Series
Pandas provides an effective way to apply a function to every element of a Series and get a new Series. Let us assume we have the following Series:

    >>> import pandas as pd
    >>> s = pd.Series([3, 7, 5, 8, 9, 1, 0, 4])
    >>> s
    0    3
    1    7
    2    5
    3    8
    4    9
    5    1
    6    0
    7    4
    dtype: int64

and a square function:

    >>> def square(x):
    ...     return x*x

We can simply apply square to every element of s and get a new Series:

    >>> t = s.apply(square)
    >>> t
    0     9
    1    49
    2    25
    3    64
    4    81
    5     1
    6     0
    7    16
    dtype: int64

In some cases it is easier to use a lambda expression:

    >>> s.apply(lambda x: x ** 2)
    0     9
    1    49
    2    25
    3    64
    4    81
    5     1
    6     0
    7    16
    dtype: int64

or we can use any builtin function:

    >>> q = pd.Series(['Bob', 'Jack', 'Rose'])
    >>> q.apply(str.lower)
    0     bob
    1    jack
    2    rose
    dtype: object

If all the elements of the Series are strings, there is an easier way to apply string methods:

    >>> q.str.lower()
    0     bob
    1    jack
    2    rose
    dtype: object
    >>> q.str.len()
    0    3
    1    4
    2    4

