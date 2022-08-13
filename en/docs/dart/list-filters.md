---
title: "List Filters"
slug: "list-filters"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

Dart filters lists through the `List.where` and `List.retainWhere` methods. The `where` function takes one argument: a boolean function that is applied to each element of the list. If the function evaluates to `true` then the list element is retained; if the function evaluates to `false`, the element is removed.

Calling `theList.retainWhere(foo)` is practically equivalent to setting `theList = theList.where(foo)`.


## Filtering a list of integers
`[-1, 0, 2, 4, 7, 9].where((x) => x > 2) --> [4, 7, 9]`

