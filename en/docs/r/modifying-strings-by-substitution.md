---
title: "Modifying strings by substitution"
slug: "modifying-strings-by-substitution"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

`sub` and `gsub` are used to edit strings using patterns. See [Pattern Matching and Replacement][1] for more on related functions and [Regular Expressions][2] for how to build a pattern.


  [1]: https://www.wikiod.com/r/pattern-matching-and-replacement
  [2]: https://www.wikiod.com/r/regular-expressions-regex

## Rearrange character strings using capture groups
If you want to change the order of a character strings you can use parentheses in the `pattern` to group parts of the string together. These groups can in the `replacement` argument be addresed using consecutive numbers. 

The following example shows how you can reorder a vector of names of the form "surname, forename" into a vector of the form "forename surname".

    library(randomNames) 
    set.seed(1)

    strings <- randomNames(5)
    strings
    # [1] "Sigg, Zachary"        "Holt, Jake"           "Ortega, Sandra"       "De La Torre, Nichole"
    # [5] "Perkins, Donovon"  

    sub("^(.+),\\s(.+)$", "\\2 \\1", strings)
    # [1] "Zachary Sigg"        "Jake Holt"           "Sandra Ortega"       "Nichole De La Torre"
    # [5] "Donovon Perkins"    

If you only need the surname you could just address the first pairs of parentheses.  

    sub("^(.+),\\s(.+)", "\\1", strings)
    # [1] "Sigg"        "Holt"        "Ortega"      "De La Torre" "Perkins"  



## Eliminate duplicated consecutive elements
Let's say we want to eliminate duplicated subsequence element from a string (it can be more than one). For example:

    2,14,14,14,19

and convert it into:

    2,14,19


Using `gsub`, we can achieve it:

    gsub("(\\d+)(,\\1)+","\\1", "2,14,14,14,19")
    [1] "2,14,19"

It works also for more than one different repetition, for example:

     > gsub("(\\d+)(,\\1)+", "\\1", "2,14,14,14,19,19,20,21")
    [1] "2,14,19,20,21"

Let's explain the regular expression:

1. `(\\d+)`: A group 1 delimited by () and finds any digit (at least one). Remember we need to use the double backslash (`\\`) here because for a character variable a backslash represents special escape character for literal string delimiters (`\"` or `\'`). `\d\` is equivalent to: `[0-9]`.
2. `,`: A punctuation sign: `,` (we can include spaces or any other delimiter)
3. `\\1`: An identical string to the group 1, i.e.: the repeated number. If that doesn't happen, then the pattern doesn't match.

Let's try a similar situation: eliminate consecutive repeated words:

    one,two,two,three,four,four,five,six

Then, just replace `\d` by `\w`, where `\w` matches any word character, including: 
any letter, digit or underscore. It is equivalent to `[a-zA-Z0-9_]`:

    > gsub("(\\w+)(,\\1)+", "\\1", "one,two,two,three,four,four,five,six")
    [1] "one,two,three,four,five,six"
    > 

Then, the above pattern includes as a particular case duplicated digits case.


