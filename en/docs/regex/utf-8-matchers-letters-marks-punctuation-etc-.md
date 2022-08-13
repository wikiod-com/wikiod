---
title: "UTF-8 matchers Letters, Marks, Punctuation etc."
slug: "utf-8-matchers-letters-marks-punctuation-etc"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Matching letters in different alphabets
Examples below are given in Ruby, but same matchers should be available in any modern language.

Let’s say we have the string `"AℵNaïve"`, produced by Messy Artificial Intelligence. It consists of letters, but generic `\w` matcher won’t match much:

    ▶ "AℵNaïve"[/\w+/]
    #⇒ "A"

The correct way to match Unicode letter with combining marks is to use `\X` to specify a grapheme cluster. There is a caveat for Ruby, though. Onigmo, the regex engine for Ruby, still [uses the old definition of a grapheme cluster](https://github.com/k-takata/Onigmo/issues/46). It is not yet updated to [Extended Grapheme Cluster](http://perldoc.perl.org/5.12.4/perlrebackslash.html#Misc) as defined in [Unicode Standard Annex 29](http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries).

So, for Ruby we could have a workaround: `\p{L}` will do almost fine, save for it fails on combined diacritical accent on `i`:

    ▶ "AℵNaïve"[/\p{L}+/]
    #⇒ "AℵNai"

By adding the “Mark symbols” to the expression, we can finally match everything:

    ▶ "AℵNaïve"[/[\p{L}\p{M}]+/]
    #⇒ "AℵNaïve"



