---
title: "Grep"
slug: "grep"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
 - grep [OPTIONS] PATTERN [FILE...]

## How to search a file for a pattern
To find the word **foo** in the file *bar* :

`grep foo ~/Desktop/bar`


To find all lines that **do not** contain foo in the file *bar* :

`grep –v foo ~/Desktop/bar`

To use find all words containing foo in the end (WIldcard Expansion):

`grep "*foo" ~/Desktop/bar`


