---
title: "Word Boundary"
slug: "word-boundary"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Syntax
* POSIX style, end of word: `[[:>:]]`
* POSIX style, start of word: `[[:<:]]`
* POSIX style, word boundary: `[[:<:][:>:]]`
* SVR4/GNU, end of word: `\>`
* SVR4/GNU, start of word: `\<`
* Perl/GNU, word boundary: `\b`
* Tcl, end of word: `\M`
* Tcl, start of word: `\m`
* Tcl, word boundary: `\y`
* Portable ERE, start of word: `(^|[^[:alnum:]_])`
* Portable ERE, end of word: `([^[:alnum:]_]|$)`

Additional Resources
-
* [POSIX chapter on regular expressions][POSIX]
* [Perl regular expression documentation][Perl]
* [Tcl re_syntax manual page][Tcl]
* [GNU grep backslash expressions][ggrep]
* [BSD re_format][re_format]
* [More reading][1]


  [1]: http://www.regular-expressions.info/wordboundaries.html
  [POSIX]: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html
  [Perl]: http://perldoc.perl.org/perlre.html
  [ggrep]: https://www.gnu.org/software/grep/manual/html_node/The-Backslash-Character-and-Special-Expressions.html
  [re_format]: http://man.openbsd.org/OpenBSD-current/man7/re_format.7
  [Tcl]: http://www.tcl.tk/man/tcl8.6/TclCmd/re_syntax.htm

## Word boundaries
# The `\b` metacharacter
To make it easier to find whole words, we can use the metacharacter `\b`. It marks the **beginning** *and* the **end** of an alphanumeric sequence\*. Also, since it only serves to mark this locations, it actually matches no character on its own.

*\*: It is common to call an alphanumeric sequence a word, since we can catch it's characters with a `\w` (the word characters class). This can be misleading, though, since `\w` also includes numbers and, in most flavors, the underscore.*

# Examples:
| Regex | Input | Matches? |
| ------ | ------ | ------ |
| `\bstack\b`  | `stackoverflow`   | **No**, since there's no ocurrence of the **whole** word `stack` |
| `\bstack\b` | `foo stack bar`  | **Yes**, since there's nothing before nor after `stack` |
| `\bstack\b` | `stack!overflow`  | **Yes**: there's nothing before `stack` and `!`is not a word character  |
| `\bstack` | `stackoverflow`  | **Yes**, since there's nothing before `stack` |
| `overflow\b` | `stackoverflow`  | **Yes**, since there's nothing after `overflow` |

# The `\B` metacharacter
This is the opposite of `\b`, matching against the location of every non-boundary character. Like `\b`, since it matches locations, it matches no character on its own. It is useful for finding *non* whole words.

# Examples:
| Regex | Input | Matches? |
| ------ | ------ | ------ |
| `\Bb\B`  | `abc`   | **Yes**, since `b` is not surrounded by word boundaries. |
| `\Ba\B` | `abc` | **No**, `a` has a word boundary on its left side. |
| `a\B` | `abc` | **Yes**, `a` does not have a word boundary on its right side. |
| `\B,\B` | `a,,,b` | **Yes**, it matches the second comma because [`\B` will also match the space between two non-word characters][1] (it should be noted that there is a word boundary to the left of the first comma and to the right of the second).


  [1]: http://stackoverflow.com/a/6664167/4504895

## Find patterns at the beginning or end of a word
Examine the following strings:

    foobarfoo
    bar
    foobar
    barfoo

 - the regular expression `bar` will match all four strings,
-  `\bbar\b` will only match the 2nd,
- `bar\b` will be able to match the 2nd and 3rd strings, and
- `\bbar` will match the 2nd and 4th strings.



## Match complete word
    \bfoo\b

will match the complete word with no alphanumeric and `_` preceding or following by it.

Taking from [regularexpression.info][1]

> There are three different positions that qualify as word boundaries:
> 
>  1. Before the first character in the string, if the first character is a word character.
>  2. After the last character in the string, if the last character is a word character.
>  3. Between two characters in the string, where one is a word character and the other is not a word character.

The term _word character_ here means any of the following

 1. Alphabet(`[a-zA-Z]`)
 2. Number(`[0-9]`)
 3. Underscore `_`

In short, _word character_ = `\w` = `[a-zA-Z0-9_]`


  [1]: http://www.regular-expressions.info/wordboundaries.html

## Make text shorter but don't break last word
To make long text at most N characters long but leave last word intact, use `.{0,N}\b` pattern:

    ^(.{0,N})\b.*

