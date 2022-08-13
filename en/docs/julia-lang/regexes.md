---
title: "Regexes"
slug: "regexes"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- Regex("[regex]")
- r"[regex]"
- match(needle, haystack)
- matchall(needle, haystack)
- eachmatch(needle, haystack)
- ismatch(needle, haystack)

## Parameters
| Parameter  | Details                                    |
|------------|--------------------------------------------|
| `needle`   | the `Regex` to look for in the `haystack`  |
| `haystack` | the text in which to look for the `needle` |

## Regex literals
Julia supports regular expressions<sup>1</sup>. The PCRE library is used as the regex implementation. Regexes are like a mini-language within a language. Since most languages and many text editors provide some support for regex, documentation and examples of how to use [regex][1] in general are outside the scope of this example.

It is possible to construct a `Regex` from a string using the constructor:

    julia> Regex("(cat|dog)s?")

But for convenience and easier escaping, the `@r_str` [string macro][2] can be used instead:

    julia> r"(cat|dog)s?"

<sup>1</sup>: Technically, Julia supports regexes, which are distinct from and more powerful than what are called [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) in language theory. Frequently, the term "regular expression" will be used to refer to regexes also.

  [1]: https://www.wikiod.com/regex/getting-started-with-regular-expressions
  [2]: https://www.wikiod.com/julia-lang/string-macros

## Finding matches
There are four primary useful functions for regular expressions, all of which take arguments in `needle, haystack` order. The terminology "needle" and "haystack" come from the English idiom "finding a needle in a haystack". In the context of regexes, the regex is the needle, and the text is the haystack.

The `match` function can be used to find the first match in a string:

    julia> match(r"(cat|dog)s?", "my cats are dogs")
    RegexMatch("cats", 1="cat")

The `matchall` function can be used to find all matches of a regular expression in a string:

    julia> matchall(r"(cat|dog)s?", "The cat jumped over the dogs.")
    2-element Array{SubString{String},1}:
     "cat" 
     "dogs"

The `ismatch` function returns a boolean indicating whether a match was found inside the string:

    julia> ismatch(r"(cat|dog)s?", "My pigs")
    false

    julia> ismatch(r"(cat|dog)s?", "My cats")
    true

The `eachmatch` function returns an iterator over `RegexMatch` objects, suitable for use with [`for` loops][1]:

    julia> for m in eachmatch(r"(cat|dog)s?", "My cats and my dog")
               println("Matched $(m.match) at index $(m.offset)")
           end
    Matched cats at index 4
    Matched dog at index 16


  [1]: https://www.wikiod.com/julia-lang/for-loops

## Capture groups
The substrings captured by [capture groups][1] are accessible from `RegexMatch` objects using indexing notation.

For instance, the following regex parses North American phone numbers written in `(555)-555-5555` format:

    julia> phone = r"\((\d{3})\)-(\d{3})-(\d{4})"

and suppose we wish to extract the phone numbers from a text:

    julia> text = """
           My phone number is (555)-505-1000.
           Her phone number is (555)-999-9999.
           """
    "My phone number is (555)-505-1000.\nHer phone number is (555)-999-9999.\n"

Using the `matchall` function, we can get an array of the substrings matched themselves:

    julia> matchall(phone, text)
    2-element Array{SubString{String},1}:
     "(555)-505-1000"
     "(555)-999-9999"

But suppose we want to access the area codes (the first three digits, enclosed in brackets). Then we can use the `eachmatch` iterator:

    julia> for m in eachmatch(phone, text)
               println("Matched $(m.match) with area code $(m[1])")
           end
    Matched (555)-505-1000 with area code 555
    Matched (555)-999-9999 with area code 555

Note here that we use `m[1]` because the area code is the first capture group in our regular expression. We can get all three components of the phone number as a tuple using a function:

    julia> splitmatch(m) = m[1], m[2], m[3]
    splitmatch (generic function with 1 method)

Then we can apply such a function to a particular `RegexMatch`:

    julia> splitmatch(match(phone, text))
    ("555","505","1000")

Or we could `map` it across each match:

    julia> map(splitmatch, eachmatch(phone, text))
    2-element Array{Tuple{SubString{String},SubString{String},SubString{String}},1}:
     ("555","505","1000")
     ("555","999","9999")


  [1]: https://www.wikiod.com/regex/capture-groups

