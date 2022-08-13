---
title: "Cross sections of different axes with MultiIndex"
slug: "cross-sections-of-different-axes-with-multiindex"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Selection of cross-sections using .xs
    In [1]:
    import pandas as pd
    import numpy as np
    arrays = [['bar', 'bar', 'baz', 'baz', 'foo', 'foo', 'qux', 'qux'],
              ['one', 'two', 'one', 'two', 'one', 'two', 'one', 'two']]
    idx_row = pd.MultiIndex.from_arrays(arrays, names=['Row_First', 'Row_Second'])
    idx_col = pd.MultiIndex.from_product([['A','B'], ['i', 'ii']], names=['Col_First','Col_Second'])
    df = pd.DataFrame(np.random.randn(8,4), index=idx_row, columns=idx_col)

    Out[1]:
    Col_First                    A                   B          
    Col_Second                   i        ii         i        ii
    Row_First Row_Second                                        
    bar       one        -0.452982 -1.872641  0.248450 -0.319433
              two        -0.460388 -0.136089 -0.408048  0.998774
    baz       one         0.358206 -0.319344 -2.052081 -0.424957
              two        -0.823811 -0.302336  1.158968  0.272881
    foo       one        -0.098048 -0.799666  0.969043 -0.595635
              two        -0.358485  0.412011 -0.667167  1.010457
    qux       one         1.176911  1.578676  0.350719  0.093351
              two         0.241956  1.082138 -0.516898 -0.196605

[`.xs`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.xs.html) accepts a `level` (either the name of said level or an integer), and an `axis`: 0 for rows, 1 for columns.

`.xs` is available for both `pandas.Series` and `pandas.DataFrame`.

**Selection on rows:**

    In [2]: df.xs('two', level='Row_Second', axis=0)
    Out[2]:  
    Col_First          A                   B          
    Col_Second         i        ii         i        ii
    Row_First                                         
    bar        -0.460388 -0.136089 -0.408048  0.998774
    baz        -0.823811 -0.302336  1.158968  0.272881
    foo        -0.358485  0.412011 -0.667167  1.010457
    qux         0.241956  1.082138 -0.516898 -0.196605

**Selection on columns:**

    In [3]: df.xs('ii', level=1, axis=1)
    Out[3]:
    Col_First                    A         B
    Row_First Row_Second                    
    bar       one        -1.872641 -0.319433
              two        -0.136089  0.998774
    baz       one        -0.319344 -0.424957
              two        -0.302336  0.272881
    foo       one        -0.799666 -0.595635
              two         0.412011  1.010457
    qux       one         1.578676  0.093351
              two         1.082138 -0.196605

**`.xs` only works for selection , assignment is NOT possible (getting, not setting):¨**

    In [4]: df.xs('ii', level='Col_Second', axis=1) = 0
      File "<ipython-input-10-92e0785187ba>", line 1
        df.xs('ii', level='Col_Second', axis=1) = 0
                                                   ^
    SyntaxError: can't assign to function call



## Using .loc and slicers
Unlike the `.xs` method, this allows you to assign values. Indexing using slicers is available since version `0.14.0`.

    In [1]:
    import pandas as pd
    import numpy as np
    arrays = [['bar', 'bar', 'baz', 'baz', 'foo', 'foo', 'qux', 'qux'],
              ['one', 'two', 'one', 'two', 'one', 'two', 'one', 'two']]
    idx_row = pd.MultiIndex.from_arrays(arrays, names=['Row_First', 'Row_Second'])
    idx_col = pd.MultiIndex.from_product([['A','B'], ['i', 'ii']], names=['Col_First','Col_Second'])
    df = pd.DataFrame(np.random.randn(8,4), index=idx_row, columns=idx_col)
    
    Out[1]:
    Col_First                    A                   B          
    Col_Second                   i        ii         i        ii
    Row_First Row_Second                                        
    bar       one        -0.452982 -1.872641  0.248450 -0.319433
              two        -0.460388 -0.136089 -0.408048  0.998774
    baz       one         0.358206 -0.319344 -2.052081 -0.424957
              two        -0.823811 -0.302336  1.158968  0.272881
    foo       one        -0.098048 -0.799666  0.969043 -0.595635
              two        -0.358485  0.412011 -0.667167  1.010457
    qux       one         1.176911  1.578676  0.350719  0.093351
              two         0.241956  1.082138 -0.516898 -0.196605

**Selection on rows**:

    In [2]: df.loc[(slice(None),'two'),:]
    Out[2]: 
    Col_First                    A                   B          
    Col_Second                   i        ii         i        ii
    Row_First Row_Second                                        
    bar       two        -0.460388 -0.136089 -0.408048  0.998774
    baz       two        -0.823811 -0.302336  1.158968  0.272881
    foo       two        -0.358485  0.412011 -0.667167  1.010457
    qux       two         0.241956  1.082138 -0.516898 -0.196605

**Selection on columns:**

    In [3]: df.loc[:,(slice(None),'ii')]
    Out[3]: 
    Col_First                    A         B
    Col_Second                  ii        ii
    Row_First Row_Second                    
    bar       one        -1.872641 -0.319433
              two        -0.136089  0.998774
    baz       one        -0.319344 -0.424957
              two        -0.302336  0.272881
    foo       one        -0.799666 -0.595635
              two         0.412011  1.010457
    qux       one         1.578676  0.093351
              two         1.082138 -0.196605

**Selection on both axis:**:

    In [4]: df.loc[(slice(None),'two'),(slice(None),'ii')]
    Out[4]: 
    Col_First                    A         B
    Col_Second                  ii        ii
    Row_First Row_Second                    
    bar       two        -0.136089  0.998774
    baz       two        -0.302336  0.272881
    foo       two         0.412011  1.010457
    qux       two         1.082138 -0.196605

**Assignment works (unlike `.xs`):**

    In [5]: df.loc[(slice(None),'two'),(slice(None),'ii')]=0
             df
    Out[5]: 
    Col_First                    A                   B          
    Col_Second                   i        ii         i        ii
    Row_First Row_Second                                        
    bar       one        -0.452982 -1.872641  0.248450 -0.319433
              two        -0.460388  0.000000 -0.408048  0.000000
    baz       one         0.358206 -0.319344 -2.052081 -0.424957
              two        -0.823811  0.000000  1.158968  0.000000
    foo       one        -0.098048 -0.799666  0.969043 -0.595635
              two        -0.358485  0.000000 -0.667167  0.000000
    qux       one         1.176911  1.578676  0.350719  0.093351
              two         0.241956  0.000000 -0.516898  0.000000

