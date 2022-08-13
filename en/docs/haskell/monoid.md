---
title: "Monoid"
slug: "monoid"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## An instance of Monoid for lists
    instance Monoid [a] where
        mempty  = []
        mappend = (++)

Checking the `Monoid` laws for this instance:

    mempty `mappend` x = x   <->   [] ++ xs = xs  -- prepending an empty list is a no-op

    x `mappend` mempty = x   <->   xs ++ [] = xs  -- appending an empty list is a no-op

    x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z
        <->
    xs ++ (ys ++ zs) = (xs ++ ys) ++ zs           -- appending lists is associative

## Collapsing a list of Monoids into a single value
`mconcat :: [a] -> a` is another [method of the `Monoid` typeclass](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Monoid.html#v:mconcat):

    ghci> mconcat [Sum 1, Sum 2, Sum 3]
    Sum {getSum = 6}
    ghci> mconcat ["concat", "enate"]
    "concatenate"

Its default definition is `mconcat = foldr mappend mempty`.

## Numeric Monoids
Numbers are monoidal in two ways: _addition_ with 0 as the unit, and _multiplication_ with 1 as the unit. Both are equally valid and useful in different circumstances. So rather than choose a preferred instance for numbers, there are two `newtypes`, `Sum` and `Product` to tag them for the different functionality.

    newtype Sum n = Sum { getSum :: n }

    instance Num n => Monoid (Sum n) where
        mempty = Sum 0
        Sum x `mappend` Sum y = Sum (x + y)

    newtype Product n = Product { getProduct :: n }

    instance Num n => Monoid (Product n) where
        mempty = Product 1
        Product x `mappend` Product y = Product (x * y)

This effectively allows for the developer to choose which functionality to use by wrapping the value in the appropriate `newtype`.

    Sum 3     <> Sum 5     == Sum 8
    Product 3 <> Product 5 == Product 15

## An instance of Monoid for ()
`()` is a `Monoid`. Since there is only one value of type `()`, there's only one thing `mempty` and `mappend` could do:

    instance Monoid () where
        mempty = ()
        () `mappend` () = ()

