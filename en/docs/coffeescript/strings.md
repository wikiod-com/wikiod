---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Block strings
Block strings can be used to hold formatted or indentation-sensitive text (or, if you just don't feel like escaping quotes and apostrophes). The indentation level that begins the block is maintained throughout, so you can keep it all aligned with the body of your code.

    html = """
           <strong>
             cup of coffeescript
           </strong>
           """

## Multiline strings
Multiline strings are allowed in CoffeeScript. Lines are joined by a single space unless they end with a backslash. Indentation is ignored.

    mobyDick = "Call me Ishmael. Some years ago --
      never mind how long precisely -- having little
      or no money in my purse, and nothing particular
      to interest me on shore, I thought I would sail
      about a little and see the watery part of the
      world..."


## Placeholder replacements
Placeholders can be used in strings to automatically substitute the values in the final string.

    container = "cup"
    liquid = "coffee"
    string = "Filling the #{container} with #{liquid}..."

The above String - when printed - will say: `Filling the cup with coffee...`

You can even use Coffee-script inside these placeholders 

    sentence = "#{ 22 / 7 } is a decent approximation of π"

