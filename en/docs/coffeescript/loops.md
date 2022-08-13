---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Looping a Function
The following codes will output the numbers 1 through 10 in the console, although `console.log` could be any function that accepts an input. 

# Method 1 - Standard

    for x in [1..10]
        console.log x

# Method 2 - Compact

    console.log x for x in [1..10]


