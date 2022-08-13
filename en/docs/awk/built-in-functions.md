---
title: "Built-in functions"
slug: "built-in-functions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## length([String])
> Returns the number of characters of the given *String*


**Considerations**
------------------
 - If a number is given instead a String, the result will be the length of the String representing the given number. I.e. If we execute `length(12345)` the result will be the same as `length("12345")`, that is *5*

 - If no value is given, the result will be the length of the actual row being processed, that is `length($0)`


 - It can be used inside a pattern or inside code-blocks.


**Examples**
------------------
Here are a few examples demonstrating how `length()`works

    $ cat file
    AAAAA
    BBBB
    CCCC
    DDDD
    EEEE

-- --
**Inside a  pattern**

Filter all lines with a length bigger than 4 characters

    $ awk ' length($0) > 4 ' file
    AAAAA

-- --

**Inside a code block**

Will print the size of the current line

    $ awk '{ print length($0) }' file
    5
    4
    4
    4
    4

-- --

**With no data given**

Will print the size of the current line

    $ awk '{ print length }' file
    5
    4
    4
    4
    4


Will print the size of the current line

    $ awk '{ print length() }' file
    5
    4
    4
    4
    4

-- --

**Number given instead of String**

Will print the size of the String representing the number

    $ awk '{ print length(12345) }' file
    5
    5
    5
    5
    5

-- -- 

**Fixed String given**

Will print the size of the String

    $ awk '{ print length("12345") }' file
    5
    5
    5
    5
    5


