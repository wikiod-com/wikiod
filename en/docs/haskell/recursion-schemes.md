---
title: "Recursion Schemes"
slug: "recursion-schemes"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

Functions mentioned here in examples are defined with varying degrees of abstraction in several packages, for example, [`data-fix`][1] and [`recursion-schemes`][2] (more functions here). You can view a more complete list by [searching on Hayoo][3].

  [1]: http://hackage.haskell.org/package/data-fix-0.0.3 "data-fix on Hackage"
  [2]: http://hackage.haskell.org/package/recursion-schemes-4.1.2 "recursion-schemes on Hackage"
  [3]: http://hayoo.fh-wedel.de/?query=cata+catamorphism "example query: cata+catamorphism"

## Fixed points
`Fix` takes a "template" type and ties the recursive knot, layering the template like a lasagne.

    newtype Fix f = Fix { unFix :: f (Fix f) }

Inside a `Fix f` we find a layer of the template `f`. To fill in `f`'s parameter, `Fix f` plugs in _itself_. So when you look inside the template `f` you find a recursive occurrence of `Fix f`.

Here is how a typical recursive datatype can be translated into our framework of templates and fixed points. We remove recursive occurrences of the type and mark their positions using the `r` parameter.

    {-# LANGUAGE DeriveFunctor #-}

    -- natural numbers
    -- data Nat = Zero | Suc Nat
    data NatF r = Zero_ | Suc_ r deriving Functor
    type Nat = Fix NatF

    zero :: Nat
    zero = Fix Zero_
    suc :: Nat -> Nat
    suc n = Fix (Suc_ n)


    -- lists: note the additional type parameter a
    -- data List a = Nil | Cons a (List a)
    data ListF a r = Nil_ | Cons_ a r deriving Functor
    type List a = Fix (ListF a)

    nil :: List a
    nil = Fix Nil_
    cons :: a -> List a -> List a
    cons x xs = Fix (Cons_ x xs)


    -- binary trees: note two recursive occurrences
    -- data Tree a = Leaf | Node (Tree a) a (Tree a)
    data TreeF a r = Leaf_ | Node_ r a r deriving Functor
    type Tree a = Fix (TreeF a)

    leaf :: Tree a
    leaf = Fix Leaf_
    node :: Tree a -> a -> Tree a -> Tree a
    node l x r = Fix (Node_ l x r)

## Folding up a structure one layer at a time
_Catamorphisms_, or _folds_, model primitive recursion. `cata` tears down a fixpoint layer by layer, using an _algebra_ function (or _folding function_) to process each layer. `cata` requires a `Functor` instance for the template type `f`.

    cata :: Functor f => (f a -> a) -> Fix f -> a
    cata f = f . fmap (cata f) . unFix

    -- list example
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr f z = cata alg
        where alg Nil_ = z
              alg (Cons_ x acc) = f x acc

## Unfolding a structure one layer at a time
_Anamorphisms_, or _unfolds_, model primitive corecursion. `ana` builds up a fixpoint layer by layer, using a _coalgebra_ function (or _unfolding function_) to produce each new layer. `ana` requires a `Functor` instance for the template type `f`.

    ana :: Functor f => (a -> f a) -> a -> Fix f
    ana f = Fix . fmap (ana f) . f

    -- list example
    unfoldr :: (b -> Maybe (a, b)) -> b -> List a
    unfoldr f = ana coalg
        where coalg x = case f x of
                             Nothing -> Nil_
                             Just (x, y) -> Cons_ x y

Note that `ana` and `cata` are _dual_. The types and implementations are mirror images of one another.

## Unfolding and then folding, fused
It's common to structure a program as building up a data structure and then collapsing it to a single value. This is called a _hylomorphism_ or _refold_. It's possible to elide the intermediate structure altogether for improved efficiency.

    hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
    hylo f g = g . fmap (hylo f g) . f  -- no mention of Fix!

Derivation:

    hylo f g = cata g . ana f
             = g . fmap (cata g) . unFix . Fix . fmap (ana f) . f  -- definition of cata and ana
             = g . fmap (cata g) . fmap (ana f) . f  -- unfix . Fix = id
             = g . fmap (cata g . ana f) . f  -- Functor law
             = g . fmap (hylo f g) . f  -- definition of hylo

## Primitive recursion
_Paramorphisms_ model primitive recursion. At each iteration of the fold, the folding function receives the subtree for further processing.

    para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
    para f = f . fmap (\x -> (x, para f x)) . unFix

The Prelude's `tails` can be modelled as a paramorphism.

    tails :: List a -> List (List a)
    tails = para alg
        where alg Nil_ = cons nil nil  -- [[]]
              alg (Cons_ x (xs, xss)) = cons (cons x xs) xss  -- (x:xs):xss

## Primitive corecursion
_Apomorphisms_ model primitive corecursion. At each iteration of the unfold, the unfolding function may return either a new seed or a whole subtree.

    apo :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
    apo f = Fix . fmap (either id (apo f)) . f

Note that `apo` and `para` are _dual_. The arrows in the type are flipped; the tuple in `para` is dual to the `Either` in `apo`, and the implementations are mirror images of each other.

