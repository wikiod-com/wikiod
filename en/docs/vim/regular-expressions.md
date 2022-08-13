---
title: "Regular expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

execute `:h pattern` to see a lot of regex related information

## Word
Vim has special operators to match word beginning, word, end, and so forth.`\<` represents the beginning of a word and `\>` represents the end of a word.

Searching for `/\<foo\>` in the following text will only return the last foo.

 > football is not foolish `foo`

