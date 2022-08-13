---
title: "Overloaded Literals"
slug: "overloaded-literals"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Integer Literals
is a numeral **without** a decimal point

for example `0`, `1`, `42`, ...

is implicitly applied to [`fromInteger`](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:fromInteger) which is part of the [`Num` type class](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Num) so it indeed has type `Num a => a` - that is it can have any type that is an instance of `Num`

---

## Fractional Literals
is a numeral **with** a decimal point

for example `0.0`, `-0.1111`, ...

is implicitly applied to [`fromRational`](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:fromRational) which is part of the [`Fractional` type class](https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Fractional) so it indeed has type ` a => a` - that is it can have any type that is an instance of `Fractional`

---

## String Literals

If you add the language extension `OverloadedStrings` to *GHC* you can have the same for `String`-literals which then are applied to [`fromString`](https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-String.html#v:fromString) from the [`Data.String.IsString` type class](https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-String.html#v:fromString)

This is often used to replace `String` with `Text` or `ByteString`.

---

## List Literals

Lists can defined with the `[1, 2, 3]` literal syntax.  In GHC 7.8 and beyond, this can also be used to define other list-like structures with the [`OverloadedLists`][1] extension.

By default, the type of `[]` is:

    > :t []
    [] :: [t]

With `OverloadedLists`, this becomes:

    [] :: GHC.Exts.IsList l => l


  [1]: https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists

## Strings
## The type of the literal

Without any extensions, the type of a string literal – i.e., something between double quotes – is just a string, aka list of characters:

    Prelude> :t "foo"
    "foo" :: [Char]

However, when the `OverloadedStrings` extension is enabled, string literals become polymorphic, similar [to number literals](https://www.wikiod.com/haskell/overloaded-literals#Integer Numeral):

    Prelude> :set -XOverloadedStrings
    Prelude> :t "foo"
    "foo" :: Data.String.IsString t => t

This allows us to define values of string-like types without the need for any explicit conversions. In essence, the `OverloadedStrings` extension just wraps every string literal in the generic [`fromString`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-String.html#v:fromString) conversion function, so if the context demands e.g. the more efficient [`Text`](http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html) instead of `String`, you don't need to worry about that yourself.

## Using string literals

    {-# LANGUAGE OverloadedStrings #-}
    
    import Data.Text (Text, pack)
    import Data.ByteString (ByteString, pack)
    
    
    withString :: String
    withString = "Hello String"
    
    -- The following two examples are only allowed with OverloadedStrings

    withText :: Text
    withText = "Hello Text"      -- instead of: withText = Data.Text.pack "Hello Text"
    
    withBS :: ByteString
    withBS = "Hello ByteString"  -- instead of: withBS = Data.ByteString.pack "Hello ByteString"

Notice how we were able to construct values of `Text` and `ByteString` in the same way we construct ordinary `String` (or `[Char]`) Values, rather than using each types `pack` function to encode the string explicitly.


For more information on the `OverloadedStrings` language extension, see [the extension documentation][1].


  [1]: https://www.wikiod.com/haskell/common-ghc-language-extensions#OverloadedStrings

## Floating Numeral
## The type of the literal

    Prelude> :t 1.0
    1.0 :: Fractional a => a

## Choosing a concrete type with annotations

You can specify the type with a *type annotation*. The only requirement is that the type must have a `Fractional` instance.

    Prelude> 1.0 :: Double
    1.0
    it :: Double
    Prelude> 1.0 :: Data.Ratio.Ratio Int
    1 % 1
    it :: GHC.Real.Ratio Int 

if not the compiler will complain

    Prelude> 1.0 :: Int
    <interactive>:
        No instance for (Fractional Int) arising from the literal `1.0'
        In the expression: 1.0 :: Int
        In an equation for `it': it = 1.0 :: Int



## Integer Numeral
## The type of the literal

    Prelude> :t 1
    1 :: Num a => a

## choosing a concrete type with annotations

You can specify the type as long as the target type is `Num` with an *annotation*:

    Prelude> 1 :: Int
    1
    it :: Int
    Prelude> 1 :: Double
    1.0
    it :: Double
    Prelude> 1 :: Word
    1
    it :: Word

if not the compiler will complain

Prelude> 1 :: String

    <interactive>:
        No instance for (Num String) arising from the literal `1'
        In the expression: 1 :: String
        In an equation for `it': it = 1 :: String

## List Literals
GHC's [OverloadedLists][1] extension allows you to construct list-like data structures with the list literal syntax.

This allows you to [Data.Map][2] like this:

    > :set -XOverloadedLists
    > import qualified Data.Map as M
    > M.lookup "foo" [("foo", 1), ("bar", 2)]
    Just 1

Instead of this (note the use of the extra [M.fromList][3]):

    > import Data.Map as M
    > M.lookup "foo" (M.fromList [("foo", 1), ("bar", 2)])
    Just 1


  [1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html#overloaded-lists
  [2]: http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html
  [3]: http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#v:fromList

