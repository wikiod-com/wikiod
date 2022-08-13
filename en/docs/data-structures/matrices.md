---
title: "Matrices"
slug: "matrices"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This article aims to explain what a matrix is and how to use it.

## Intro to matrices
Matrices are essentially two-dimensional arrays.

That means that it associates (i, j) coordinates, where i is the row and j is the column, to a value.

So, you could have :

m<sub>3, 4</sub> = "Hello"

The easiest implementation is to make an array or arrays. In python, that would go as follows.

<!-- language: lang-python -->
    matrix = [["a", "b", "c"], ["d", "e", "f"], ["g", "h", "i"]]
    print(matrix[1][2]) #returns "f"    

