---
title: "Meta Documentation Guidelines"
slug: "meta-documentation-guidelines"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

This meta post is similar to the python version https://www.wikiod.com/python

Please make edit suggestions, and comment on those (in lieu of proper comments), so we can flesh out/iterate on these suggestions :)

## style
Use the pandas library as `pd`, this can be assumed (the import does not need to be in every example)

    import pandas as pd

[PEP8!](https://www.python.org/dev/peps/pep-0008/)

- 4 space indentation
- kwargs should use no spaces `f(a=1)`
- 80 character limit (the entire line fitting in the rendered code snippet should be strongly preferred)


## Pandas version support
Most examples will work across multiple versions, if you are using a "new" feature you should mention when this was introduced.

Example: `sort_values`.

## Showing code snippets and output
Two popular options are to use:

ipython notation:

```
In [11]: df = pd.DataFrame([[1, 2], [3, 4]])

In [12]: df
Out[12]:
   0  1
0  1  2
1  3  4
```

Alternatively (this is popular over in the python documentation) and more concisely:

```
df.columns  # Out: RangeIndex(start=0, stop=2, step=1)

df[0]
# Out:
# 0    1
# 1    3
# Name: 0, dtype: int64

for col in df:
    print(col)
# prints:
# 0
# 1
```

*Generally, this is better for smaller examples.*

Note: The distinction between output and printing. ipython makes this clear (the prints occur before the output is returned):

```
In [21]: [print(col) for col in df]
0
1
Out[21]: [None, None]
```

## print statements
Most of the time printing should be avoided as it can be a distraction (Out should be preferred).  
That is:

```
a
# Out: 1
```
is always better than 
```
print(a)
# prints: 1
```

## Prefer supporting python 2 and 3:

    print(x)    # yes! (works same in python 2 and 3)
    print x     # no! (python 2 only)
    print(x, y) # no! (works differently in python 2 and 3)

