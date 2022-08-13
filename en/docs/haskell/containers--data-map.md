---
title: "Containers - Data.Map"
slug: "containers---datamap"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Monoid instance
`Map k v` provides a https://www.wikiod.com/haskell/type-classes#Monoid instance with the following semantics:

* `mempty` is the empty `Map`, i.e. the same as [`Map.empty`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:empty)
* `m1 <> m2` is the left-biased union of `m1` and `m2`, i.e. if any key is present both in `m1` and `m2`, then the value from `m1` is picked for `m1 <> m2`. This operation is also available outside the `Monoid` instance as [`Map.union`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:union).

## Constructing

We can create a Map from a list of tuples like this:

```haskell
Map.fromList [("Alex", 31), ("Bob", 22)]
```

A Map can also be constructed with a single value:

```haskell
> Map.singleton "Alex" 31
fromList [("Alex",31)]
```

There is also the `empty` function.

```haskell
empty :: Map k a
```

Data.Map also supports typical set operations such as [`union`][4], [`difference`][5] and [`intersection`][6].


  [1]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#g:4
  [2]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:findWithDefault
  [3]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#g:6
  [4]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:union
  [5]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:difference
  [6]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:intersection

## Checking If Empty
We use the `null` function to check if a given Map is empty:

```haskell
> Map.null $ Map.fromList [("Alex", 31), ("Bob", 22)]
False

> Map.null $ Map.empty
True
```

## Finding Values
There are [many][1] querying operations on maps. 

`member :: Ord k => k -> Map k a -> Bool` yields `True` if the key of type `k` is in `Map k a`:

```haskell
> Map.member "Alex" $ Map.singleton "Alex" 31
True
> Map.member "Jenny" $ Map.empty
False
```

`notMember` is similar:

```haskell
> Map.notMember "Alex" $ Map.singleton "Alex" 31
False
> Map.notMember "Jenny" $ Map.empty
True
```

You can also use [`findWithDefault :: Ord k => a -> k -> Map k a -> a`][2] to yield a default value if the key isn't present:

```haskell
Map.findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
Map.findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
```

  [1]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#g:4
  [2]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:findWithDefault


## Inserting Elements
[Inserting][3] elements is simple:

```haskell
> let m = Map.singleton "Alex" 31
fromList [("Alex",31)]

> Map.insert "Bob" 99 m
fromList [("Alex",31),("Bob",99)]
```

  [3]: https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#g:6


## Deleting Elements

```haskell
> let m = Map.fromList [("Alex", 31), ("Bob", 99)]
fromList [("Alex",31),("Bob",99)]

> Map.delete "Bob" m
fromList [("Alex",31)]
```


## Importing the Module
The `Data.Map` module in the [`containers` package](https://hackage.haskell.org/package/containers) provides a `Map` structure that has both strict and lazy implementations.

When using `Data.Map`, one usually imports it qualified to avoid clashes with functions already defined in Prelude:

```haskell
import qualified Data.Map as Map
```

So we'd then prepend `Map` function calls with `Map.`, e.g.

    Map.empty -- give me an empty Map


