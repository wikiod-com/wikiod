---
title: "Calculating Big-O"
slug: "calculating-big-o"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## O(n) functions.
Functions that are `O(n)` increase the number of operations linearly, as the input gets very large.  A simple example of a function that is O(n) would be the linear search algorithm, which runs once for the size of the input.

The following pseudo-code would be `O(n)`, because it will always be bounded above by the input size, as the algorithm will never run more times then the input size.

    function LinearSearch (SearchArray, SearchFor)
        for each element in SearchArray
            if the element is SearchFor
                return the index of element



