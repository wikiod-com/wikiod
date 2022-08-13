---
title: "Working with Time Series"
slug: "working-with-time-series"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Creating Time Series
Here is how to create a simple Time Series.

    import pandas as pd
    import numpy as np

    # The number of sample to generate
    nb_sample = 100
    
    # Seeding to obtain a reproductible dataset
    np.random.seed(0)
    
    se = pd.Series(np.random.randint(0, 100, nb_sample),
                      index = pd.date_range(start = pd.to_datetime('2016-09-24'),
                                            periods = nb_sample, freq='D'))
    se.head(2)
    
    # 2016-09-24    44
    # 2016-09-25    47
    
    se.tail(2)

    # 2016-12-31    85
    # 2017-01-01    48




## Partial String Indexing
A very handy way to subset Time Series is to use **partial string indexing**. It permits to select range of dates with a clear syntax.

# Getting Data

We are using the dataset in the [Creating Time Series](https://www.wikiod.com/pandas/working-with-time-series#Creating Time Series) example

Displaying head and tail to see the boundaries

    se.head(2).append(se.tail(2))
    
    # 2016-09-24    44
    # 2016-09-25    47
    # 2016-12-31    85
    # 2017-01-01    48
        
# Subsetting

Now we can subset by year, month, day very intuitively.

By year

    se['2017']
    
    # 2017-01-01    48
   
By month

    se['2017-01']
    
    # 2017-01-01    48

By day

    se['2017-01-01']

    # 48

With a range of year, month, day according to your needs.

    se['2016-12-31':'2017-01-01']
    
    # 2016-12-31    85
    # 2017-01-01    48

pandas also provides a dedicated `truncate` function for this usage through the `after` and `before` parameters -- but I think it's less clear.

    se.truncate(before='2017')

    # 2017-01-01    48

    se.truncate(before='2016-12-30', after='2016-12-31')

    # 2016-12-30    13
    # 2016-12-31    85

