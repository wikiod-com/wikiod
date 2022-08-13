---
title: "Making Pandas Play Nice With Native Python Datatypes"
slug: "making-pandas-play-nice-with-native-python-datatypes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Moving Data Out of Pandas Into Native Python and Numpy Data Structures
    In [1]: df = pd.DataFrame({'A': [1, 2, 3], 'B': [1.0, 2.0, 3.0], 'C': ['a', 'b', 'c'], 
                           'D': [True, False, True]})

    In [2]: df
    Out[2]: 
       A    B  C      D
    0  1  1.0  a   True
    1  2  2.0  b  False
    2  3  3.0  c   True

Getting a python list from a series:

    In [3]: df['A'].tolist()
    Out[3]: [1, 2, 3]

DataFrames do not have a `tolist()` method. Trying it results in an AttributeError:

    In [4]: df.tolist()
    ---------------------------------------------------------------------------
    AttributeError                            Traceback (most recent call last)
    <ipython-input-4-fc6763af1ff7> in <module>()
    ----> 1 df.tolist()
    
    //anaconda/lib/python2.7/site-packages/pandas/core/generic.pyc in __getattr__(self, name)
       2742             if name in self._info_axis:
       2743                 return self[name]
    -> 2744             return object.__getattribute__(self, name)
       2745 
       2746     def __setattr__(self, name, value):
    
    AttributeError: 'DataFrame' object has no attribute 'tolist'


Getting a numpy array from a series:

    In [5]: df['B'].values
    Out[5]: array([ 1.,  2.,  3.])

You can also get an array of the columns as individual numpy arrays from an entire dataframe:

    In [6]: df.values
    Out[6]: 
    array([[1, 1.0, 'a', True],
           [2, 2.0, 'b', False],
           [3, 3.0, 'c', True]], dtype=object)

Getting a dictionary from a series (uses the index as the keys):

    In [7]: df['C'].to_dict()
    Out[7]: {0: 'a', 1: 'b', 2: 'c'}

You can also get the entire DataFrame back as a dictionary:

    In [8]: df.to_dict()
    Out[8]: 
    {'A': {0: 1, 1: 2, 2: 3},
     'B': {0: 1.0, 1: 2.0, 2: 3.0},
     'C': {0: 'a', 1: 'b', 2: 'c'},
     'D': {0: True, 1: False, 2: True}}

The `to_dict` method has a few different parameters to adjust how the dictionaries are formatted. To get a list of dicts for each row:

    In [9]: df.to_dict('records')
    Out[9]: 
    [{'A': 1, 'B': 1.0, 'C': 'a', 'D': True},
     {'A': 2, 'B': 2.0, 'C': 'b', 'D': False},
     {'A': 3, 'B': 3.0, 'C': 'c', 'D': True}]
See [the documentation][1] for the full list of options available to create dictionaries.

  [1]: http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.to_dict.html

