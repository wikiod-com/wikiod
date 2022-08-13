---
title: "Reading a DataFrame from a file"
slug: "reading-a-dataframe-from-a-file"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Reading a dataframe from delimiter separated data
You may want to read a `DataFrame` from a CSV (Comma separated values) file or maybe even from a TSV or WSV (tabs and whitespace separated files). If your file has the right extension, you can use the `readtable` function to read in the dataframe:

    readtable("dataset.CSV")

But what if your file doesn't have the right extension? You can specify the delimiter that your file uses (comma, tab, whitespace etc) as a keyword argument to the `readtable` function:

    readtable("dataset.txt", separator=',')

## Handling different comment comment marks
Data sets often contain comments that explain the data format or contain the license and usage terms. You usually want to ignore these lines when you read in the `DataFrame`.

The `readtable` function assumes that comment lines begin with the '#' character. However, your file may use comment marks like `%` or `//`. To make sure that `readtable` handles these correctly, you can specify the comment mark as a keyword argument:

    readtable("dataset.csv", allowcomments=true, commentmark='%')

