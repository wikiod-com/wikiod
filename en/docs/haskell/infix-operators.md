---
title: "Infix operators"
slug: "infix-operators"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Most Haskell functions are called with the function name followed by arguments (prefix notation). For functions that accept two arguments like (+), it sometimes makes sense to provide an argument before and after the function (infix).

## Prelude
Logical
===

[`&&`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-38--38-) is logical AND, [`||`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-124--124-) is logical OR.

[`==`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-61--61-) is equality, [`/=`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-47--61-) non-equality, [`<`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-60-) / `<=` lesser and `>` / `>=` greater operators.

Arithmetic operators
===

The numerical operators [`+`](http://hackage.haskell.org/package/base/docs/Prelude.html#v:-43-), [`-`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-45-) and [`/`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-47-) behave largely as you'd expect. (Division works only on fractional numbers to avoid rounding issues – integer division must be done with [`quot`](http://hackage.haskell.org/package/base/docs/Prelude.html#v:quot) or [`div`](http://hackage.haskell.org/package/base/docs/Prelude.html#v:div)). More unusual are Haskell's three exponentiation operators:

- [`^`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-94-) takes a base of any number type to a non-negative, integral power. This works simply by ([fast](https://en.wikipedia.org/wiki/Exponentiation_by_squaring)) iterated multiplication. E.g.

      4^5  ≡  (4*4)*(4*4)*4

- [`^^`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-94-) does the same in the positive case, but also works for negative exponents. E.g.

      3^^(-2)  ≡  1 / (2*2)

  Unlike `^`, this requires a fractional base type (i.e. `4^^5 :: Int` will not work, only `4^5 :: Int` or `4^^5 :: Rational`).

- [`**`](http://hackage.haskell.org/package/base/docs/Prelude.html#v:-42--42-) implements real-number exponentiation. This works for very general arguments, but is more computionally expensive than `^` or `^^`, and generally incurs small floating-point errors.

      2**pi  ≡  exp (pi * log 2)

Lists
===

There are two concatenation operators:

- `:` (pronounced [cons](https://en.wikipedia.org/wiki/Cons)) prepends a single argument before a list. This operator is actually a constructor and can thus also be used to _pattern match_ (“inverse construct”) a list.
- [`++`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-43--43-) concatenates entire lists.

      [1,2] ++ [3,4]  ≡  1 : 2 : [3,4]  ≡  1 : [2,3,4]  ≡  [1,2,3,4]

[`!!`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-33--33-) is an indexing operator.

    [0, 10, 20, 30, 40] !! 3  ≡  30

Note that indexing lists is inefficient (complexity _O_(_n_) instead of _O_(1) for [arrays](http://hackage.haskell.org/package/vector/docs/Data-Vector.html) or _O_(log _n_) for [maps](http://hackage.haskell.org/package/containers/docs/Data-Map.html)); it's generally preferred in Haskell to deconstruct lists by folding ot pattern matching instead of indexing.

Control flow
===

- [`$`](http://hackage.haskell.org/package/base/docs/Prelude.html#v:-36-) is a function application operator.

      f $ x  ≡  f x
             ≡  f(x)  -- disapproved style

  This operator is mostly used to avoid parentheses. It also has a strict version [`$!`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-36--33-), which forces the argument to be evaluated before applying the function.

- [`.`](http://hackage.haskell.org/package/base/docs/Prelude.html#v:-36-) composes functions.

      (f . g) x  ≡  f (g x)  ≡  f $ g x

- [`>>`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-62--62-) sequences monadic actions. E.g. `writeFile "foo.txt" "bla" >> putStrLn "Done."` will first write to a file, then print a message to the screen. 
- [`>>=`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:-62--62--61-) does the same, while also accepting an argument to be passed from the first action to the following. `readLn >>= \x -> print (x^2)` will wait for the user to input a number, then output the square of that number to the screen.

## Finding information about infix operators
Because infixes are so common in Haskell, you will regularly need to look up their signature etc.. Fortunately, this is just as easy as for any other function:

- The Haskell search engines [Hayoo](https://hayoo.fh-wedel.de/) and [Hoogle](https://www.haskell.org/hoogle/) can be used for infix operators, like for anything else that's defined in some library.

- In GHCi or IHaskell, you can use the `:i` and `:t` (**i**nfo and **t**ype) directives to learn the basic properties of an operator. For example,

      Prelude> :i +
      class Num a where
        (+) :: a -> a -> a
        ...
            -- Defined in ‘GHC.Num’
      infixl 6 +
      Prelude> :i ^^
      (^^) :: (Fractional a, Integral b) => a -> b -> a
            -- Defined in ‘GHC.Real’
      infixr 8 ^^

  This tells me that `^^` binds more tightly than `+`, both take numerical types as their elements, but `^^` requires the exponent to be integral and the base to be fractional.<br>The less verbose `:t` requires the operator in parentheses, like

      Prelude> :t (==)
      (==) :: Eq a => a -> a -> Bool

## Custom operators
In Haskell, you can define any infix operator you like. For example, I could define the list-enveloping operator as

    (>+<) :: [a] -> [a] -> [a]
    env >+< l = env ++ l ++ env

    GHCi> "**">+<"emphasis"
    "**emphasis**"

You should always give such operators a [fixity declaration](https://www.wikiod.com/haskell/fixity-declarations), like

    infixr 5 >+<

(which would mean `>+<` binds as tightly as `++` and `:` do).

