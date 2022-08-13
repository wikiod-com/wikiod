---
title: "Data.Text"
slug: "datatext"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

`Text` is a more efficient alternative to Haskell's standard `String` type. `String` is defined as a linked list of characters in the standard Prelude, per [the Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1190006.1.2):

    type String = [Char]

`Text` is represented as a packed array of Unicode characters. This is similar to how most other high-level languages represent strings, and gives much better time and space efficiency than the list version.

`Text` should be preferred over `String` for all production usage. A notable exception is depending on a library which has a `String` API, but even in that case there may be a benefit of using `Text` internally and converting to a `String` just before interfacing with the library.

All of the examples in this topic use [the `OverloadedStrings` language extension](https://www.wikiod.com/haskell/common-ghc-language-extensions#OverloadedStrings).

## Text Literals
The [`OverloadedStrings`](https://www.wikiod.com/haskell/overloaded-literals#Strings) language extension allows the use of normal string literals to stand for `Text` values.

    {-# LANGUAGE OverloadedStrings #-}
    
    import qualified Data.Text as T
    
    myText :: T.Text
    myText = "overloaded"

## Checking if a Text is a substring of another Text
    ghci> :set -XOverloadedStrings
    ghci> import Data.Text as T

[`isInfixOf :: Text -> Text -> Bool`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html#v:isInfixOf) checks whether a `Text` is contained anywhere within another `Text`.

    ghci> "rum" `T.isInfixOf` "crumble"
    True

[`isPrefixOf :: Text -> Text -> Bool`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html#v:isPrefixOf) checks whether a `Text` appears at the beginning of another `Text`.

    ghci> "crumb" `T.isPrefixOf` "crumble"
    True

[`isSuffixOf :: Text -> Text -> Bool`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html#v:isSuffixOf) checks whether a `Text` appears at the end of another `Text`.

    ghci> "rumble" `T.isSuffixOf` "crumble"
    True

## Stripping whitespace
    {-# LANGUAGE OverloadedStrings #-}
    
    import qualified Data.Text as T
    
    myText :: T.Text
    myText = "\n\r\t   leading and trailing whitespace   \t\r\n"

`strip` removes whitespace from the start and end of a `Text` value.

    ghci> T.strip myText
    "leading and trailing whitespace"

`stripStart` removes whitespace only from the start.

    ghci> T.stripStart myText
    "leading and trailing whitespace   \t\r\n"

`stripEnd` removes whitespace only from the end.

    ghci> T.stripEnd myText
    "\n\r\t   leading and trailing whitespace"

`filter` can be used to remove whitespace, or other characters, from the middle. 
    
    ghci> T.filter /=' ' "spaces in the middle of a text string"
    "spacesinthemiddleofatextstring"

## Splitting Text Values
    {-# LANGUAGE OverloadedStrings #-}
    
    import qualified Data.Text as T
    
    myText :: T.Text
    myText = "mississippi"

`splitOn` breaks a `Text` up into a list of `Texts` on occurrences of a substring.

    ghci> T.splitOn "ss" myText
    ["mi","i","ippi"]

`splitOn` is the inverse of `intercalate`.

    ghci> intercalate "ss" (splitOn "ss" "mississippi")
    "mississippi"

`split` breaks a `Text` value into chunks on characters that satisfy a Boolean predicate.

    ghci> T.split (== 'i') myText
    ["m","ss","ss","pp",""]

## Encoding and Decoding Text
Encoding and decoding functions for a variety of Unicode encodings can be found in the `Data.Text.Encoding` module.

    ghci> import Data.Text.Encoding
    ghci> decodeUtf8 (encodeUtf8 "my text")
    "my text"

Note that `decodeUtf8` will throw an exception on invalid input. If you want to handle invalid UTF-8 yourself, use `decodeUtf8With`.

    ghci> decodeUtf8With (\errorDescription input -> Nothing) messyOutsideData

## Indexing Text
    {-# LANGUAGE OverloadedStrings #-}

    import qualified Data.Text as T
    
    myText :: T.Text
    
    myText = "mississippi"

Characters at specific indices can be returned by the `index` function.

    ghci> T.index myText 2
    's'
The `findIndex` function takes a function of type `(Char -> Bool)` and Text and returns the index of the first occurrence of a given string or Nothing if it doesn't occur.

    ghci> T.findIndex ('s'==) myText
    Just 2
    ghci> T.findIndex ('c'==) myText
    Nothing
The `count` function returns the number of times a query `Text` occurs within another `Text`.

    ghci> count ("miss"::T.Text) myText
    1

 

