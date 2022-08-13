---
title: "Boolean indexing of dataframes"
slug: "boolean-indexing-of-dataframes"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Accessing rows in a dataframe using the DataFrame indexer objects `.ix`, `.loc`, `.iloc` and how it differentiates itself from using a boolean mask.


## Applying a boolean mask to a dataframe
This will be our example data frame:

      color      name   size
    0   red      rose    big
    1  blue    violet    big
    2   red     tulip  small
    3  blue  harebell  small

Using the magic `__getitem__` or `[]` accessor. Giving it a list of True and False of the same length as the dataframe will give you:

    df[[True, False, True, False]]
      color   name   size
    0   red   rose    big
    2   red  tulip  small





## Accessing a DataFrame with a boolean index
This will be our example data frame:

    df = pd.DataFrame({"color": ['red', 'blue', 'red', 'blue']},
                      index=[True, False, True, False])
          color
    True    red
    False  blue
    True    red
    False  blue

Accessing with `.loc`

    df.loc[True]
         color
    True   red
    True   red

Accessing with `.iloc`

    df.iloc[True]
    >> TypeError

    df.iloc[1]
    color    blue
    dtype: object

>Important to note is that older pandas versions did not distinguish between boolean and integer input, thus `.iloc[True]` would return the same as `.iloc[1]`

Accessing with `.ix`

    df.ix[True]
         color
    True   red
    True   red
    
    df.ix[1]
    color    blue
    dtype: object

As you can see, `.ix` has two behaviors. This is very bad practice in code and thus it should be avoided. Please use `.iloc` or `.loc` to be more explicit.

## Masking data based on column value
This will be our example data frame:

      color      name   size
    0   red      rose    big
    1  blue    violet  small
    2   red     tulip  small
    3  blue  harebell  small

Accessing a single column from a data frame, we can use a simple comparison `==` to compare every element in the column to the given variable, producing a `pd.Series` of True and False

    df['size'] == 'small'
    0    False
    1     True
    2     True
    3     True
    Name: size, dtype: bool

This `pd.Series` is an extension of an `np.array` which is an extension of a simple `list`, Thus we can hand this to the `__getitem__` or `[]` accessor as in the above example.

    size_small_mask = df['size'] == 'small'
    df[size_small_mask]
      color      name   size
    1  blue    violet  small
    2   red     tulip  small
    3  blue  harebell  small

## Masking data based on index value
This will be our example data frame:

             color   size
    name                 
    rose       red    big
    violet    blue  small
    tulip      red  small
    harebell  blue  small

We can create a mask based on the index values, just like on a column value.

    rose_mask = df.index == 'rose'
    df[rose_mask]
         color size
    name           
    rose   red  big

But doing this is *almost* the same as

    df.loc['rose']
    color    red
    size     big
    Name: rose, dtype: object

The important difference being, when `.loc` only encounters one row in the index that matches, it will return a `pd.Series`, if it encounters more rows that matches, it will return a `pd.DataFrame`. This makes this method rather unstable.

This behavior can be controlled by giving the `.loc` a list of a single entry. This will force it to return a data frame.

    df.loc[['rose']]
             color   size
    name                 
    rose       red    big





