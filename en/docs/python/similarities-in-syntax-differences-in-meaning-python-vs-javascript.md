---
title: "Similarities in syntax, Differences in meaning Python vs. JavaScript"
slug: "similarities-in-syntax-differences-in-meaning-python-vs-javascript"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

It sometimes happens that two languages put different meanings on the same or similar syntax expression. When the both languages are of interest for a programmer,  clarifying these bifurcation points helps to better understand the both languages in their basics and subtleties.

## `in` with lists
    2 in [2, 3]

In Python this evaluates to True, but in JavaScript to false. This is because in Python in checks if a value is contained in a list, so 2 is in [2, 3] as its first element. In JavaScript in is used with objects and checks if an object contains the property with the name expressed by the value. So JavaScript considers [2, 3] as an object or a key-value map like this:

    {'0': 2, '1': 3}

and checks if it has a property or a key '2' in it. Integer 2 is silently converted to string '2'.

