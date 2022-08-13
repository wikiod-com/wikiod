---
title: "Characters can consist of multiple code points"
slug: "characters-can-consist-of-multiple-code-points"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

An Unicode code point, what programmers often think of one character, often corresponds to what the user thinks is one character. Sometimes however a “character” is made up of multiple code points, as the examples above show.

This means that operations like slicing a string, or getting a character at a given index may not work as expected. For instance the 4<sup>th</sup> character of the string `"Café"` is `'e'` (without the accent). Similarly, clipping the string to length 4 will remove the accent.

The technical term for such a group of code points is a *grapheme cluster*. See [UAX #29: Unicode Text Segmentation](http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries)

## Diacritics
A letter with a diacritic may be represented with the letter, and a combining modifier letter. You normally think of `é` as one character, but it's really 2 code points:

 - `U+0065` — LATIN SMALL LETTER E
 - `U+0301` — COMBINING ACUTE ACCENT

Similarly `ç` = `c` + `¸`, and `å` = `a` + `˚`

## combined forms

To complicate matters, there is often a code point for the composed form as well:

    "Café" = 'C' + 'a' + 'f' + 'e' + '´'
    "Café" = 'C' + 'a' + 'f' + 'é'

Although these strings look the same, they are not equal, and they don't even have the same length (5 and 4 respectively).

## Emoji and flags
A lot of emoji consist of more than one code point.

 - 🇯🇵: A flag is defined as a pair of "regional symbol indicator letters" (🇯 + 🇵)
 - 🙋🏿: Some emoji may be followed by a skin tone modifier: 🙋 + 🏿
 - 😀︎ or 😀️: Windows 10 allows you to specify if an emoji is colored or black/white by appending a variation selector (`U+FE0E` or `U+FE0F`)
 - 👨‍👩‍👧‍👦: a family. Encoded by joining the emoji for boy, girl, woman and man (👦, 👧, 👩, 👨) together with zero-width joiners (`U+200D`). On platforms which support it, this is rendered as an emoji of a family with two kids.


## Zalgo Text
There is this thing called [Zalgo Text](http://stackoverflow.com/questions/6579844/how-does-zalgo-text-work) which pushes this to the extreme. Here is the first grapheme cluster of the example. It consists of 15 code points: the Latin letter `H` and 14 combining marks.

>&nbsp;
>
>H̡̫̤̤̣͉̤ͭ̓̓̇͗̎̀
>
> &nbsp;

Although this doesn't show up in normal text, it shows that a “character” really can consist of an arbitrary number of code points

