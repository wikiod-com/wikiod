---
title: "Recursion"
slug: "recursion"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

Recursion is mostly available in Perl-compatible flavors, such as:

 - Perl
 - PCRE
 - Oniguruma
 - Boost
 

## Recurse into a subpattern
You can recurse into a subpattern using the following constructs (depending on the flavor), assuming `n` is a capturing group number, and `name` the name of a capturing group.

 - `(?n)`
 - `\g<n>`
 - `\g'0'`
 - `(?&name)`
 - `\g<name>`
 - `\g'name'`
 - `(?P>name)`
 
The following pattern:

     \[(?<angle><(?&angle)*+>)*\]
    
Will match text such as: `[<<><>><>]` - well balanced angle brackets within square brackets. Recursion is often used for balanced constructs matching.

## Recursions are atomic (PCRE)
In PCRE, it doesn't trackback after the first match for a recursion is found. So

    (?(DEFINE)(aaa|aa|a))(?1)ab

doesn't match

    aab

because after it matched `aa` in the recursion, it never try again to match only `a`.

## Recurse the whole pattern
The construct `(?R)` is equivalent to `(?0)` (or `\g<0>`) - it lets you recurse the whole pattern:

    <(?>[^<>]+|(?R))+>
    
This will match properly balanced angle brackets with any text in-between the brackets, like `<a<b>c<d>e>`.


## Subpattern definitions
The `(?(DEFINE)`...`)` construct lets you define subpatterns you may reference later through recursion. When encountered in the pattern it will *not* be matched against.

This group should contain named subpattern definitions, which will be accessible only through recursion. You can define grammars this way:

    (?x) # ignore pattern whitespace
    (?(DEFINE)
      (?<string> ".*?" )
      (?<number> \d+ )
      (?<value>
        \s* (?:
            (?&string)
          | (?&number)
          | (?&list)
        ) \s*
      )
      (?<list> \[ (?&value) (?: , (?&value) )* \] )
    )
    ^(?&value)$
    
This pattern will validate text like the following:

    [42, "abc", ["foo", "bar"], 10]

Note how a list can contain one or more values, and a value can itself be a list.


## Relative group references
Subpatterns can be referenced with their *relative* group number:

 - `(?-1)` will recurse into the *previous* group
 - `(?+1)` will recurse into the *next* group
 
Also usable with the `\g<N>` syntax.


## Backreferences in recursions (PCRE)
In PCRE, matched groups used for backreferences before a recursion are kept in the recursion. But after the recursion they all reset to what they were before entering it. In other words, matched groups in the recursion are all forgotten.

For example:

    (?J)(?(DEFINE)(\g{a}(?<a>b)\g{a}))(?<a>a)\g{a}(?1)\g{a}

matches

    aaabba

