---
title: "Regular expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Regular expressions


## Look behind
Given the following file:

    hello how are you
    i am fine
    let's go, you!
    let's go, baby!

`grep` with [look-behind][1] allows to print only some parts:

    $ grep -Po "(?<=let's go, ).*" file
    you!
    baby!

In this case, it matches what occurs after "let's go, ".

  [1]: https://www.wikiod.com/regex/lookahead-and-lookbehind

