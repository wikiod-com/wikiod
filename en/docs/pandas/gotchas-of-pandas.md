---
title: "Gotchas of pandas"
slug: "gotchas-of-pandas"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

Gotcha in general is a construct that is although documented, but not intuitive. Gotchas produce some output that is normally not expected because of its counter-intuitive character. 

Pandas package has several gotchas, that can confuse someone, who is not aware of them, and some of them are presented on this documentation page.

## Detecting missing values with np.nan
If you want to detect missings with

    df=pd.DataFrame({'col':[1,np.nan]})
    df==np.nan
you will get the following result: 

    col
    0    False
    1    False
This is because comparing missing value to anything results in a False - instead of this you should use 

    df=pd.DataFrame({'col':[1,np.nan]})   
    df.isnull()
which results in: 

    col
    0    False
    1    True




## Integer and NA
Pandas don't support missing in attributes of type integer. For example if you have missings in the grade column:

    df= pd.read_csv("data.csv", dtype={'grade': int}) 
    error: Integer column has NA values
In this case you just should use float instead of integers or set the object dtype. 


## Automatic Data Alignment (index-awared behaviour)
If you want to append a series of values [1,2] to the column of dataframe df, you will get NaNs:

    import pandas as pd
    
    series=pd.Series([1,2])
    df=pd.DataFrame(index=[3,4])
    df['col']=series
    df

       col
    3    NaN
    4    NaN
because setting a new column automatically aligns the data by the indexe, and your values 1 and 2 would get the indexes 0 and 1, and not 3 and 4 as in your data frame:

    df=pd.DataFrame(index=[1,2])
    df['col']=series
    df
    
       col
    1      2.0
    2      NaN

If you want to ignore index, you should set the .values at the end: 

    df['col']=series.values

       col
    3    1
    4    2




