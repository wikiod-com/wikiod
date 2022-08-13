---
title: "Categorical data"
slug: "categorical-data"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

**Categoricals** are a pandas data type, which correspond to categorical variables in statistics: a variable, which can take on only a limited, and usually fixed, number of possible values (categories; levels in R). 

Examples are gender, social class, blood types, country affiliations, observation time or ratings via Likert scales.

Source: [Pandas Docs](http://pandas.pydata.org/pandas-docs/stable/categorical.htm)

## Object Creation
    
    In [188]: s = pd.Series(["a","b","c","a","c"], dtype="category")
    
    In [189]: s
    Out[189]: 
    0    a
    1    b
    2    c
    3    a
    4    c
    dtype: category
    Categories (3, object): [a, b, c]
    
    In [190]: df = pd.DataFrame({"A":["a","b","c","a", "c"]})
    
    In [191]: df["B"] = df["A"].astype('category')
    
    In [192]: df["C"] = pd.Categorical(df["A"])
    
    In [193]: df
    Out[193]: 
       A  B  C
    0  a  a  a
    1  b  b  b
    2  c  c  c
    3  a  a  a
    4  c  c  c
    
    In [194]: df.dtypes
    Out[194]: 
    A      object
    B    category
    C    category
    dtype: object


## Creating large random datasets
    In [1]: import pandas as pd
            import numpy as np
    
    In [2]: df = pd.DataFrame(np.random.choice(['foo','bar','baz'], size=(100000,3)))
            df = df.apply(lambda col: col.astype('category'))
    
    In [3]: df.head()
    Out[3]: 
         0    1    2
    0  bar  foo  baz
    1  baz  bar  baz
    2  foo  foo  bar
    3  bar  baz  baz
    4  foo  bar  baz
    
    In [4]: df.dtypes
    Out[4]:
    0    category
    1    category
    2    category
    dtype: object
    
    In [5]: df.shape
    Out[5]: (100000, 3)



