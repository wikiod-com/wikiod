---
title: "Normal mode commands"
slug: "normal-mode-commands"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
 - :[range]sor[t][!] [b][f][i][n][o][r][u][x] [/{pattern}/]

 - Note: Options [n][f][x][o][b] are mutually exclusive.


See [sorting](http://vimhelp.appspot.com/change.txt.html#sorting) in the vim manual for the canonical explanation

## Sorting text
Normal sorting
==============
Highlight the text to sort, and the type:

`:sort`

If you don't highlight text or specify a range, the whole buffer is sorted.

Reverse sorting
===============
`:sort!`

Case insensitive sorting
===============
`:sort i`

Numerical sorting
===============
Sort by the first number to appear on each line:

`:sort n`



Remove duplicates after sorting
===============
`:sort u`
(u stands for unique)

Combining options
==================
To get a reverse case-insensitive sort with duplicates removed:

`:sort! iu`

