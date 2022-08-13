---
title: "Using .ix, .iloc, .loc, .at and .iat to access a DataFrame"
slug: "using-ix-iloc-loc-at-and-iat-to-access-a-dataframe"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Using .iloc
.iloc uses integers to read and write data to a DataFrame. 

First,  let's create a DataFrame:

    df = pd.DataFrame({'one': [1, 2, 3, 4, 5],
                       'two': [6, 7, 8, 9, 10],
                  }, index=['a', 'b', 'c', 'd', 'e'])

This DataFrame looks like: 

       one  two
    a    1    6
    b    2    7
    c    3    8
    d    4    9
    e    5   10

Now we can use .iloc to read and write values. Let's read the first row, first column: 

    print df.iloc[0, 0]

This will print out:

    1

We can also set values. Lets set the second column, second row to something new:

    df.iloc[1, 1] = '21'

And then have a look to see what happened:

    print df 

       one two
    a    1   6
    b    2  21
    c    3   8
    d    4   9
    e    5  10





## Using .loc
.loc uses **labels** to read and write data. 

Let's setup a DataFrame:

    df = pd.DataFrame({'one': [1, 2, 3, 4, 5],
                       'two': [6, 7, 8, 9, 10],
                      }, index=['a', 'b', 'c', 'd', 'e'])

Then we can print the DataFrame to have a look at the shape:

    print df 

This will output 

       one  two
    a    1    6
    b    2    7
    c    3    8
    d    4    9
    e    5   10

We use the column and row **labels** to access data with .loc. Let's set row 'c', column 'two' to the value 33: 

    df.loc['c', 'two'] = 33 

This is what the DataFrame now looks like:
 
       one  two
    a    1    6
    b    2    7
    c    3   33
    d    4    9
    e    5   10

Of note, using `df['two'].loc['c'] = 33` may not report a warning, and may even work, however, using `df.loc['c', 'two']` is guaranteed to work correctly, while the former is not.

We can read slices of data, for example

    print df.loc['a':'c'] 

will print rows a to c. This is inclusive. 

       one  two
    a    1    6
    b    2    7
    c    3    8

And finally, we can do both together: 

    print df.loc['b':'d', 'two']

Will output rows b to c of column 'two'. Notice that the column label is not printed. 

    b    7
    c    8
    d    9

If .loc is supplied with an integer argument that is not a label it reverts to integer indexing of axes (the behaviour of .iloc). This makes mixed label and integer indexing possible:

    df.loc['b', 1]

will return the value in 2nd column (index starting at 0) in row 'b':

    7

   



