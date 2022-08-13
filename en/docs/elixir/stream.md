---
title: "Stream"
slug: "stream"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Streams are composable, lazy enumerables.

Due to their laziness, streams are useful when working with large (or even infinite) collections. When chaining many operations with `Enum`, intermediate lists are created, while `Stream` creates a recipe of computations that are executed at a later moment.

## Chaining multiple operations
`Stream` is especially useful when you want to run multiple operations on a collection. This is because `Stream` is lazy and only does one iteration (whereas `Enum` would do multiple iterations, for example).

```
numbers = 1..100
|> Stream.map(fn(x) -> x * 2 end)
|> Stream.filter(fn(x) -> rem(x, 2) == 0 end)
|> Stream.take_every(3)
|> Enum.to_list

[2, 8, 14, 20, 26, 32, 38, 44, 50, 56, 62, 68, 74, 80, 86, 92, 98, 104, 110,
 116, 122, 128, 134, 140, 146, 152, 158, 164, 170, 176, 182, 188, 194, 200]
```

Here, we chained 3 operations (`map`, `filter` and `take_every`), but the final iteration was only done after `Enum.to_list` was called.

What `Stream` does internally, is that it waits until actual evaluation is required. Before that, it creates a list of all the functions, but once evaluation is needed, it does goes through the collection once, running all the functions on every item. This makes it more efficient than `Enum`, which in this case would do 3 iterations, for example.

