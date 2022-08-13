---
title: "String manipulation"
slug: "string-manipulation"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Regular expressions
    # Extract strings with a specific regex
    df= df['col_name'].str.extract[r'[Aa-Zz]']

    # Replace strings within a regex
    df['col_name'].str.replace('Replace this', 'With this')

For information on how to match strings using regex, see [Getting started with Regular Expressions][1].


  [1]: https://www.wikiod.com/regex/getting-started-with-regular-expressions

## Capitalization of strings
    In [1]: ser = pd.Series(['lORem ipSuM', 'Dolor sit amet', 'Consectetur Adipiscing Elit'])

Convert all to uppercase:

    In [2]: ser.str.upper()
    Out[2]:
    0                    LOREM IPSUM
    1                 DOLOR SIT AMET
    2    CONSECTETUR ADIPISCING ELIT
    dtype: object

All lowercase:

    In [3]: ser.str.lower()
    Out[3]:
    0                    lorem ipsum
    1                 dolor sit amet
    2    consectetur adipiscing elit
    dtype: object

Capitalize the first character and lowercase the remaining:

    In [4]: ser.str.capitalize()
    Out[4]:
    0                    Lorem ipsum
    1                 Dolor sit amet
    2    Consectetur adipiscing elit
    dtype: object


Convert each string to a titlecase (capitalize the first character of each word in each string, lowercase the remaining):

    In [5]: ser.str.title()
    Out[5]:
    0                    Lorem Ipsum
    1                 Dolor Sit Amet
    2    Consectetur Adipiscing Elit
    dtype: object

Swap cases (convert lowercase to uppercase and vice versa):

    In [6]: ser.str.swapcase()
    Out[6]:
    0                    LorEM IPsUm
    1                 dOLOR SIT AMET
    2    cONSECTETUR aDIPISCING eLIT
    dtype: object


Aside from these methods that change the capitalization, several methods can be used to check the capitalization of strings.

    In [7]: ser = pd.Series(['LOREM IPSUM', 'dolor sit amet', 'Consectetur Adipiscing Elit'])

Check if it is all lowercase:

    In [8]: ser.str.islower()
    Out[8]:
    0    False
    1     True
    2    False
    dtype: bool

Is it all uppercase:

    In [9]: ser.str.isupper()
    Out[9]:
    0     True
    1    False
    2    False
    dtype: bool

Is it a titlecased string:

    In [10]: ser.str.istitle()
    Out[10]:
    0    False
    1    False
    2     True
    dtype: bool

## Checking for contents of a string
`str.contains()` method can be used to check if a pattern occurs in each string of a Series. `str.startswith()`  and `str.endswith()` methods can also be used as more specialized versions.

    In [1]: animals = pd.Series(['cat', 'dog', 'bear', 'cow', 'bird', 'owl', 'rabbit', 'snake'])

Check if strings contain the letter 'a':

    In [2]: animals.str.contains('a')
    Out[2]:
    0      True
    1     False
    2      True
    3     False
    4     False
    5     False
    6      True
    7      True
    8      True
    dtype: bool

This can be used as a boolean index to return only the animals containing the letter 'a':

    In [3]: animals[animals.str.contains('a')]
    Out[3]: 
    0       cat
    2      bear
    6    rabbit
    7     snake
    dtype: object

`str.startswith` and `str.endswith` methods work similarly, but they also accept tuples as inputs.

    In [4]: animals[animals.str.startswith(('b', 'c'))]
    # Returns animals starting with 'b' or 'c'
    Out[4]: 
    0     cat
    2    bear
    3     cow
    4    bird
    dtype: object

## Slicing strings
Strings in a Series can be sliced using `.str.slice()` method, or more conveniently, using brackets (`.str[]`). 

    In [1]: ser = pd.Series(['Lorem ipsum', 'dolor sit amet', 'consectetur adipiscing elit'])
    In [2]: ser
    Out[2]: 
    0                    Lorem ipsum
    1                 dolor sit amet
    2    consectetur adipiscing elit
    dtype: object 

Get the first character of each string:

    In [3]: ser.str[0]
    Out[3]: 
    0    L
    1    d
    2    c
    dtype: object

Get the first three characters of each string:

    In [4]: ser.str[:3]
    Out[4]: 
    0    Lor
    1    dol
    2    con
    dtype: object

Get the last character of each string:

    In [5]: ser.str[-1]
    Out[5]:
    0    m
    1    t
    2    t
    dtype: object

Get the last three characters of each string:

    In [6]: ser.str[-3:]
    Out[6]: 
    0    sum
    1    met
    2    lit
    dtype: object

Get the every other character of the first 10 characters:

    In [7]: ser.str[:10:2]
    Out[7]: 
    0    Lrmis
    1    dlrst
    2    cnett
    dtype: object

Pandas behaves similarly to Python when handling slices and indices. For example, if an index is outside the range, Python raises an error:

    In [8]:'Lorem ipsum'[12]
    # IndexError: string index out of range

However, if a slice is outside the range, an empty string is returned:

    In [9]: 'Lorem ipsum'[12:15]
    Out[9]: ''

Pandas returns NaN when an index is out of range:

    In [10]: ser.str[12]
    Out[10]:
    0    NaN
    1      e
    2      a
    dtype: object

And returns an empty string if a slice is out of range:

    In [11]: ser.str[12:15]
    Out[11]:
    0       
    1     et
    2    adi
    dtype: object

