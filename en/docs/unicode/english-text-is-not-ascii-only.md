---
title: "English text is not ASCII only"
slug: "english-text-is-not-ascii-only"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

An assumption which pops up regularly is that when dealing with English text only, it’s unlikely to encounter characters outside the ASCII character set. To avoid problems with handling Unicode correctly, people are tempted to do things like stripping non-ASCII characters, or removing any accents on letters.

These examples show this assumption is wrong, and even for English text you should take care to handle Unicode characters correctly.

## Diacritics
English text has the occasional diacritics.


- Loan words, like née, café, entrée
- Names, like Noël and Chloë
- Place names, like  Montréal and Québec
    

## Punctuation
Almost all written text has punctuation marks which are outside the ASCII character set:

 - dashes: the en dash –, and the em dash —
 - Quotation marks: “quotes” rather than "quotes"
 - The ellipsis…

## Special symbols
There are a few common symbols in use:

 - copyright sign ©, and trademark signs ® ™
 - fractions like ¼
 - superscripts. For instance, a shorthand for square meters is m².


## Emoji
Emoji are quite popular with social media these days. 

 - ☃: `U+2603` — SNOWMAN
 - 😀: `U+01F600` — GRINNING FACE
 - 🐪: `U+01F42A` — DROMEDARY CAMEL

Note that most emoji are outside the Basic Multilingual Plane. A lot of newer additions consist of more than one code point:

 - 🇯🇵: A flag is defined as a pair of "regional symbol indicator letters"
 - 🙋🏿: This is an emoji plus a skin tone modifier: 🙋 + 🏿
 - 😀︎ or 😀️: Windows 10 allows you to specify if an emoji is colored or black/white by appending a variation selector (`U+FE0E` or `U+FE0F`)

