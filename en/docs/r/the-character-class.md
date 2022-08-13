---
title: "The character class"
slug: "the-character-class"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Characters are what other languages call 'string vectors.'

# Related topics 

Patterns
- https://www.wikiod.com/r/regular-expressions-regex 
- https://www.wikiod.com/r/pattern-matching-and-replacement
- https://www.wikiod.com/r/strsplit-function

Input and output
- https://www.wikiod.com/r/reading-and-writing-strings


## Coercion
To check whether a value is a character use the `is.character()` function. To coerce a variable to a character use the `as.character()` function.
```
x <- "The quick brown fox jumps over the lazy dog"
class(x)
[1] "character"
is.character(x)
[1] TRUE
```

Note that numerics can be coerced to characters, but attempting to coerce a character to numeric may result in `NA`.

```
as.numeric("2")
[1] 2
as.numeric("fox")
[1] NA
Warning message:
NAs introduced by coercion 
```


