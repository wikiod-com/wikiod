---
title: "Row Manipulation"
slug: "row-manipulation"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Extract specific lines from a text file
Suppose we have a file

    cat -n lorem_ipsum.txt
     1    Lorem Ipsum is simply dummy text of the printing and typesetting industry.
     2    Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.
     3    It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.
     4    It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum

We want to extract lines 2 and 3 from this file

    awk 'NR==2,NR==3' lorem_ipsum.txt

This will print lines 2 and 3:

     2    Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.
     3    It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged.

## Extract specific column/field from specific line
If you have the following data file 

    cat data.csv
    1 2 3 4 5 6 7 8 9 10
    11 12 13 14 15 16 17 18 19 20
    21 22 23 24 25 26 27 28 29 30
    31 32 33 34 35 36 37 38 39 40
    41 42 43 44 45 46 47 48 49 50

maybe you need to read the fourth column of the third line, this would be "24"

    awk 'NR==3 { print $4 }' data.csv

gives

> 24



## Modifying rows on-the-fly (e.g. to fix Windows line-endings)
If a file may contain Windows or Unix-like line endings (or even a mixture of both) then the intended text replacement may not work as expected.

Sample:

    $ echo -e 'Entry 1\nEntry 2.1\tEntry 2.2\r\nEntry 3\r\n\r\n' \
    > | awk -F'\t' '$1 != "" { print $1 }' \
    > | hexdump -c
    0000000   E   n   t   r   y       1  \n   E   n   t   r   y       2   .
    0000010   1  \n   E   n   t   r   y       3  \r  \n  \r  \n            
    000001d

This can be easily fixed by an additional rule which is inserted at the beginning of the awk script:

    /\r$/ { $0 = substr($0, 1, length($0) - 1) }

Because the action does not end with `next`, the following rules are applied as before.

Sample (with fix of line-endings):

    $ echo -e 'Entry 1\nEntry 2.1\tEntry 2.2\r\nEntry 3\r\n\r\n' \
    > | awk -F'\t' '/\r$/ { $0 = substr($0, 1, length($0) - 1) } $1 != "" { print $1 }' \
    > | hexdump -c
    0000000   E   n   t   r   y       1  \n   E   n   t   r   y       2   .
    0000010   1  \n   E   n   t   r   y       3  \n                        
    000001a


