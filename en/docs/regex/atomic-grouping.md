---
title: "Atomic Grouping"
slug: "atomic-grouping"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Regular non-capturing groups allow the engine to re-enter the group and attempt to match something different (such as a different alternation, or match fewer characters when a quantifier is used).

Atomic groups differ from regular non-capturing groups in that backtracking is forbidden. Once the group exits, all backtracking information is discarded, so no alternate matches can be attempted.

A [possessive quantifier](https://www.wikiod.com/regex/possessive-quantifiers) behaves like an atomic group in that the engine will be unable to backtrack over a token or group.

The following are equivalent in terms of functionality, although some will be faster than others:

    a*+abc
    (?>a*)abc
    (?:a+)*+abc
    (?:a)*+abc
    (?:a*)*+abc
    (?:a*)++abc


## Grouping with (?>)
<h2>Using an Atomic Group</h2>

Atomic groups have the format `(?>...)` with a `?>` after the open paren.

Consider the following sample text:

    ABC

The regex will attempt to match starting at position 0 of the text, which is before the `A` in `ABC`.

If a case-insensitive expression `(?>a*)abc` were used, the `(?>a*)` would match 1 `A` character, leaving

    BC

as the remaining text to match. The `(?>a*)` group is exited, and `abc` is attempted on the remaining text, which fails to match.

The engine is unable to backtrack into the atomic group, and so the current pass fails. The engine moves to the next position in the text, which would be at position 1, which is after the `A` and before the `B` of `ABC`.

The regex `(?>a*)abc` is attempted again, and `(?>a*)` matches `A` 0 times, leaving

    BC

as the remaining text to match. The `(?>a*)` group is exited and `abc` is attempted, which fails.

Again, the engine is unable to backtrack into the atomic group, and so the current pass fails. The regex will continue to fail until all positions in the text have been exhausted.






<h2>Using a Non-Atomic Group</h2>

Regular non-capturing groups have the format `(?:...)` with a `?:` after the open paren.

Given the same sample text, but with the case-insensitive expression `(?:a*)abc` instead, a match would occur since backtracking is allowed to occur.

At first, `(?:a*)` will consume the letter `A` in the text

    ABC

leaving

    BC

as the remaining text to match. The `(?:a*)` group is exited, and `abc` is attempted on the remaining text, which fails to match.

The engine backtracks into the `(?:a*)` group and attempts to match 1 fewer character: Instead of matching 1 `A` character, it attempts to match 0 `A` characters, and the `(?:a*)` group is exited. This leaves

    ABC

as the remaining text to match. The regex `abc` is now able to successfully match the remaining text.






<h2>Other Example Text</h2>

Consider this sample text, with both atomic and non-atomic groups (again, case-insensitive):

    AAAABC

The regex will attempt to match starting at position 0 of the text, which is before the first `A` in `AAAABC`.

The pattern using the atomic group `(?>a*)abc` will be **unable** to match, behaving almost identically to the atomic `ABC` example above: all 4 of the `A` characters are first matched with `(?>a*)` (leaving `BC` as the remaining text to match), and `abc` is unable to match on that text. The group **is not** able to be re-entered, so the match fails.

The pattern using the non-atomic group `(?:a*)abc` will be **able** to match, behaving similarly to the non-atomic `ABC` example above: all 4 of the `A` characters are first matched with `(?:a*)` (leaving `BC` as the remaining text to match), and `abc` is unable to match on that text. The group **is** able to be re-entered, so one fewer `A` is attempted: 3 `A` characters are matched instead of 4 (leaving `ABC` as the remaining text to match), and `abc` is able to successfully match on that text.

