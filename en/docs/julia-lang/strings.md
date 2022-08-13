---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
- "[string]"
- '[Unicode scalar value]'
- graphemes([string])

## Parameters
| Parameter | Details |
|-----------|---------|
| **For**   | `sprint(f, xs...)` |
| `f`       | A function that takes an `IO` object as its first argument. |
| `xs`      | Zero or more remaining arguments to pass to `f`. |

## Hello, World!
Strings in Julia are delimited using the `"` symbol:

    julia> mystring = "Hello, World!"
    "Hello, World!"

Note that unlike some other languages, the `'` symbol _cannot_ be used instead. `'` defines a _character literal_; this is a `Char` data type and will only store a single [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value):

    julia> 'c'
    'c'
    
    julia> 'character'
    ERROR: syntax: invalid character literal

One can extract the unicode scalar values from a string by iterating over it with a [`for` loop][1]:

    julia> for c in "Hello, World!"
               println(c)
           end
    H
    e
    l
    l
    o
    ,
     
    W
    o
    r
    l
    d
    !


  [1]: https://www.wikiod.com/julia-lang/for-loops

## Graphemes
Julia's `Char` type represents a [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value), which only in some cases corresponds to what humans perceive as a "character". For instance, one representation of the character é, as in résumé, is actually a combination of two Unicode scalar values:

    julia> collect("é")
    2-element Array{Char,1}:
     'e'
     '́'

The Unicode descriptions for these codepoints are "LATIN SMALL LETTER E" and "COMBINING ACUTE ACCENT". Together, they define a single "human" character, which is Unicode terms is called a [grapheme](http://www.unicode.org/glossary/#grapheme). More specifically, Unicode Annex #29 motivates the definition of a [grapheme cluster](http://www.unicode.org/glossary/#grapheme_cluster) because:

> It is important to recognize that what the user thinks of as a “character”—a basic unit of a writing system for a language—may not be just a single Unicode code point. Instead, that basic unit may be made up of multiple Unicode code points. To avoid ambiguity with the computer use of the term character, this is called a user-perceived character. For example, “G” + acute-accent is a user-perceived character: users think of it as a single character, yet is actually represented by two Unicode code points. These user-perceived characters are approximated by what is called a grapheme cluster, which can be determined programmatically. 

Julia provides the `graphemes` function to iterate over the grapheme clusters in a string:

    julia> for c in graphemes("résumé")
               println(c)
           end
    r
    é
    s
    u
    m
    é

Note how the result, printing each character on its own line, is better than if we had iterated over the Unicode scalar values:

    julia> for c in "résumé"
               println(c)
           end
    r
    e
    
    s
    u
    m
    e
    
Typically, when working with characters in a user-perceived sense, it is more useful to deal with grapheme clusters than with Unicode scalar values. For instance, suppose we want to write a function to compute the length of a single word. A naïve solution would be to use

    julia> wordlength(word) = length(word)
    wordlength (generic function with 1 method)

We note that the result is counter-intuitive when the word includes grapheme clusters that consist of more than one codepoint:

    julia> wordlength("résumé")
    8

When we use the more correct definition, using the `graphemes` function, we get the expected result:

    julia> wordlength(word) = length(graphemes(word))
    wordlength (generic function with 1 method)
    
    julia> wordlength("résumé")
    6


## Convert numeric types to strings
There are numerous ways to convert numeric types to strings in Julia:

    julia> a = 123
    123

    julia> string(a)
    "123"

    julia> println(a)
    123

The `string()` function can also take more arguments:

    julia> string(a, "b")
    "123b"


You can also insert (aka interpolate) integers (and certain other types) into strings using `$`:

    julia> MyString = "my integer is $a"
    "my integer is 123"

**Performance Tip:** The above methods can be quite convenient at times.  But, if you will be performing many, many such operations and you are concerned about execution speed of your code, the Julia [performance guide](http://docs.julialang.org/en/release-0.4/manual/performance-tips/#avoid-string-interpolation-for-i-o) recommends against this, and instead in favor of the below methods:

You can supply multiple arguments to `print()` and `println()` which will operate on them exactly as `string()` operates on multiple arguments:

    julia> println(a, "b")
    123b

Or, when writing to file, you can similarly use, e.g. 

    open("/path/to/MyFile.txt", "w") do file
        println(file, a, "b", 13)
    end

or

    file = open("/path/to/MyFile.txt", "a")
    println(file, a, "b", 13)
    close(file)

These are faster because they avoid needing to first form a string from given pieces and then output it (either to the console display or a file) and instead just sequentially output the various pieces.

Credits: Answer based on SO Question [What's the best way to convert an Int to a String in Julia?](http://stackoverflow.com/questions/38676573/whats-the-best-way-to-convert-an-int-to-a-string-in-julia) with Answer by Michael Ohlrogge and Input from Fengyang Wang


## String interpolation (insert value defined by variable into string)
In Julia, as in many other languages, it is possible to interpolate by inserting values defined by variables into strings.  For a simple example:

    n = 2
    julia> MyString = "there are $n ducks"
    "there are 2 ducks"

We can use other types than numeric, e.g.

    Result = false
    julia> println("test results is $Result")
    test results is false

You can have multiple interpolations within a given string:

    MySubStr = "a32"
    MyNum = 123.31
    println("$MySubStr  ,   $MyNum")

**Performance Tip** Interpolation is quite convenient.  But, if you are going to be doing it many times very rapidly, it is not the most efficient.  Instead, see [Convert numeric types to strings](https://www.wikiod.com/julia-lang/strings#Convert numeric types to strings) for suggestions when performance is an issue.

## Using sprint to Create Strings with IO Functions
Strings can be made from functions that work with `IO` objects by using the `sprint` function. For instance, the `code_llvm` function accepts an `IO` object as the first argument. Typically, it is used like

    julia> code_llvm(STDOUT, *, (Int, Int))
    
    define i64 @"jlsys_*_46115"(i64, i64) #0 {
    top:
      %2 = mul i64 %1, %0
      ret i64 %2
    }

Suppose we want that output as a string instead. Then we can simply do

    julia> sprint(code_llvm, *, (Int, Int))
    "\ndefine i64 @\"jlsys_*_46115\"(i64, i64) #0 {\ntop:\n  %2 = mul i64 %1, %0\n  ret i64 %2\n}\n"
    
    julia> println(ans)
    
    define i64 @"jlsys_*_46115"(i64, i64) #0 {
    top:
      %2 = mul i64 %1, %0
      ret i64 %2
    }

Converting the results of "interactive" functions like `code_llvm` into strings can be useful for automated analysis, such as [testing][1] whether generated code may have regressed.

The `sprint` function is a [higher-order function][2] which takes the function operating on `IO` objects as its first argument. Behind the scenes, it creates an `IOBuffer` in RAM, calls the given function, and takes the data from the buffer into a `String` object.


  [1]: https://www.wikiod.com/julia-lang/unit-testing
  [2]: https://www.wikiod.com/julia-lang/higher-order-functions

