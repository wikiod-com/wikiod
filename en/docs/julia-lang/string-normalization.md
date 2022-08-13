---
title: "String Normalization"
slug: "string-normalization"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
- normalize_string(s::String, ...)

## Parameters
| Parameter | Details |
|-----------|---------|
| `casefold=true` | Fold the string to a canonical case based off the [Unicode](http://unicode.org/Public/UCD/latest/ucd/CaseFolding.txt) standard. |
| `stripmark=true` | Strip [diacritical marks][1] (i.e. accents) from characters in the input string. |


  [1]: https://en.wikipedia.org/wiki/Diacritic

## Case-Insensitive String Comparison
[Strings][1] can be compared with the [`==` operator][2] in Julia, but this is sensitive to differences in case. For instance, `"Hello"` and `"hello"` are considered different strings.

    julia> "Hello" == "Hello"
    true
    
    julia> "Hello" == "hello"
    false

To compare strings in a case-insensitive manner, normalize the strings by case-folding them first. For example,

    equals_ignore_case(s, t) =
        normalize_string(s, casefold=true) == normalize_string(t, casefold=true)

This approach also handles non-ASCII Unicode correctly:

    julia> equals_ignore_case("Hello", "hello")
    true
    
    julia> equals_ignore_case("Weierstraß", "WEIERSTRASS")
    true

Note that in German, the uppercase form of the ß character is SS.


  [1]: https://www.wikiod.com/julia-lang/strings
  [2]: https://www.wikiod.com/julia-lang/comparisons

## Diacritic-Insensitive String Comparison
Sometimes, one wants strings like `"resume"` and `"résumé"` to compare equal. That is, [graphemes][1] that share a basic glyph, but possibly differ because of additions to those basic glyphs. Such comparison can be accomplished by stripping diacritical marks.

    equals_ignore_mark(s, t) =
        normalize_string(s, stripmark=true) == normalize_string(t, stripmark=true)

This allows the above example to work correctly. Additionally, it works well even with non-ASCII Unicode characters.

    julia> equals_ignore_mark("resume", "résumé")
    true
    
    julia> equals_ignore_mark("αβγ", "ὰβ̂γ̆")
    true

  [1]: https://www.wikiod.com/julia-lang/strings#Graphemes

