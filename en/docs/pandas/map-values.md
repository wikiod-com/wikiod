---
title: "Map Values"
slug: "map-values"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

it should be mentioned that if the key value does not exist then this will raise `KeyError`, in those situations it maybe better to use `merge` or [`get`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.get.html#pandas.Series.get) which allows you to specify a default value if the key doesn't exist

## Map from Dictionary
Starting from a dataframe `df`:

      U   L
    111  en
    112  en
    112  es
    113  es
    113  ja
    113  zh
    114  es

Imagine you want to add a new column called `S` taking values from the following dictionary:

    d = {112: 'en', 113: 'es', 114: 'es', 111: 'en'}

You can use [`map`](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.map.html) to perform a lookup on keys returning the corresponding values as a new column:

    df['S'] = df['U'].map(d)

that returns:

      U   L   S
    111  en  en
    112  en  en
    112  es  en
    113  es  es
    113  ja  es
    113  zh  es
    114  es  es



