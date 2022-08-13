---
title: "Character classes"
slug: "character-classes"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

# Simple classes

| Regex | Matches |
| ------ | ------ |
| `[abc]`| Any of the following characters: `a`, `b`, or `c`  |
| `[a-z]`| Any character from `a` to `z`, **inclusive** (this is called a *range*)|
| `[0-9]`| Any digit from `0` to `9`, **inclusive**|

# Common classes
Some groups/ranges of characters are so often used, they have special abbreviations:

| Regex  | Matches |
| ------ | ----- |
| `\w`   | Alphanumeric characters plus the underscore (also referred to as "word characters") |
| `\W`   | Non-word characters (same as `[^\w]`)|
| `\d`   | Digits (*wider* than `[0-9]` since include Persian digits, Indian ones etc.)|
| `\D`   | Non-digits (*shorter* than `[^0-9]` since reject Persian digits, Indian ones etc.)
| `\s`   | Whitespace characters (spaces, tabs, etc...) **Note**: may vary depending on your engine/context
| `\S`   | Non-whitespace characters

# Negating classes
 A [caret (^)][1] after the opening square bracket works as a negation of the characters that follow it. This will match all characters that are not in the character class.

Negated character classes also match line break characters, therefore if these are not to be matched, the specific line break characters must be added to the class (\r and/or \n).

| Regex | Matches |
| ------ | ------ |
| `[^AB]`   | Any character **other** than `A` and `B`   |
| `[^\d]` | Any character, **except** digits |


  [1]: https://www.wikiod.com/regex/anchor-characters-caret-

## Character class and common problems faced by beginner
**1. Character Class**

Character class is denoted by `[]`. Content inside a character class is treated as `single character separately`. e.g. suppose we use

    [12345]

In the example above, it means match `1 or 2 or 3 or 4 or 5` . In simple words, it can be understood as `or condition for single characters` (**stress on single character**)

**1.1 Word of caution**

- In character class, there is no concept of matching a string. So, if you are using regex `[cat]`, it does not mean that it should match the word `cat` literally but it means that it should match either `c` or `a` or `t`. This is a very common misunderstanding existing among people who are newer to regex.
- Sometimes people use `|` (alternation) inside character class thinking it will act as `OR condition` which is wrong. e.g. using `[a|b]` actually means match `a` or `|` (literally) or `b`.

**2. Range in character class**

Range in character class is denoted using `-` sign. Suppose we want to find any character within English alphabets `A` to `Z`. This can be done by using the following character class

    [A-Z]

This could be done for any valid ASCII or unicode range. Most commonly used ranges include `[A-Z]`, `[a-z]` or `[0-9]`. Moreover these ranges can be combined in character class as

    [A-Za-z0-9]

This means that match any character in the range `A to Z` or `a to z` or `0 to 9`. The ordering can be anything. So the above is equivalent to `[a-zA-Z0-9]` as long as the range you define is correct.

**2.1 Word of caution**

 - Sometimes when writing ranges for `A` to `Z` people write it as `[A-z]`. This is wrong  in most cases because we are using `z` instead of `Z`. So this denotes match any character from ASCII range `65` (of A) to `122` (of z) which includes many unintended character after ASCII range `90` (of Z). **HOWEVER**, `[A-z]` can be used to match all `[a-zA-Z]` letters in POSIX-style regex when collation is set for a particular language. 
`[[ "ABCEDEF[]_abcdef" =~ ([A-z]+) ]] && echo "${BASH_REMATCH[1]}"` on Cygwin with `LC_COLLATE="en_US.UTF-8"` yields `ABCEDF`.
If you set `LC_COLLATE` to `C` (on Cygwin, done with `export`), it will give the expected `ABCEDEF[]_abcdef`.

 - Meaning of `-` inside character class is special. It denotes range as explained above. *What if we want to match `-` character literally?* We can't put it anywhere otherwise it will denote ranges if it is put between two characters. In that case we have to put `-` in starting of character class like `[-A-Z]` or in end of character class like `[A-Z-]` or `escape it` if you want to use it in middle like `[A-Z\-a-z]`.

**3. Negated character class**

Negated character class is denoted by `[^..]`. The caret sign `^` denotes match any character except the one present in character class. e.g.

    [^cat]

means match any character except `c` or `a` or `t`.

**3.1 Word of caution**

- The meaning of caret sign `^` maps to negation only if its in the starting of character class. If its anywhere else in character class it is treated as literal caret character without any special meaning.
- Some people write regex like `[^]`. In most regex engines, this gives an error. The reason being when you are using `^` in the starting position, it expects at least one character that should be negated. In *JavaScript* though, this is a valid construct matching *anything but nothing*, i.e. matches any possible symbol (but diacritics, at least in ES5).



## The basics
Suppose we have a list of teams, named like this: `Team A`, `Team B`, ..., `Team Z`. Then:
- `Team [AB]`: This will match either either `Team A` or `Team B`
- `Team [^AB]`: This will match any team **except** `Team A` or `Team B`

We often need to match characters that "belong" together in some context or another (like letters from `A` through `Z`), and this is what character classes are for.

## Match different, similar words
Consider the character class `[aeiou]`. This character class can be used in a regular expression to match a set of similarly spelled words.

`b[aeiou]t` matches:
 - bat
 - bet
 - bit
 - bot
 - but

It does not match:
 - bout
 - btt
 - bt

Character classes on their own match one and only one character at a time.

## Non-alphanumerics matching (negated character class)
    [^0-9a-zA-Z]

This will match all characters that are neither numbers nor letters (alphanumerical characters). If the underscore character `_` is also to be negated, the expression can be shortened to:

    [^\w]
    
Or:

    \W

In the following sentences:

> 1. Hi, what's up?
>
> 2. I can't wait for 2017!!!

The following characters match:
> 1. `,`, ` `, `'`, `?` and the end of line character.
>
> 2. `'`, ` `, `!` and the end of line character.
    
**UNICODE NOTE**<br/>
Note that some flavors with Unicode character properties support may interpret `\w` and `\W` as `[\p{L}\p{N}_]` and `[^\p{L}\p{N}_]` which means other Unicode letters and numeric characters will be included as well (see [PCRE docs](http://pcre.org/pcre.txt)). Here is a [PCRE `\w` test][1]:[![enter image description here][2]][2]



In .NET, [`\w` = `[\p{Ll}\p{Lu}\p{Lt}\p{Lo}\p{Lm}\p{Mn}\p{Nd}\p{Pc}]`][3], and note it does not match `\p{Nl}` and `\p{No}` unlike PCRE (see the [`\w` .NET documentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-classes-in-regular-expressions#WordCharacter)):




[![Picture][4]][4]

Note that for some reason, Unicode 3.1 lowercase letters (like `𝐚𝒇𝓌𝔨𝕨𝗐𝛌𝛚`) are not matched.

Java's `(?U)\w` will match a  mix of what `\w` matches in PCRE and .NET:[![enter image description here][5]][5]


  [1]: https://regex101.com/r/Z7OKm0/1/
  [2]: https://i.stack.imgur.com/ryTcm.png
  [3]: http://regexstorm.net/tester?p=%5e%5cw%2b&i=%5cw+%3d+%5b%5cp%7bLl%7d%5cp%7bLu%7d%5cp%7bLt%7d%5cp%7bLo%7d%5cp%7bLm%7d%5cp%7bMn%7d%5cp%7bNd%7d%5cp%7bPc%7d%5d+%28no+%5cp%7bNl%7d%2c+%5cp%7bNo%7d+as+PCRE%29%0d%0a%ef%ac%94%c4%85%d1%84r%ef%bd%97%e1%bd%a3%e1%bb%b7%e1%b5%ba%e1%b4%94%e1%b4%89%d5%be%d4%8d%d3%b9%d3%a1%d2%81%ca%ab+-+Ll%2c+lowercase+letters+%28some%29%0d%0aA%c3%82%c4%9e%c6%8e%c6%97%c7%8a%ce%94%ce%98%ce%a3%cf%a2%d0%89%d0%a9%d1%ac%d3%b2%d4%bd%e1%82%b5%e1%8e%bf%e1%8f%89%e1%8f%af%d4%8c%e2%84%ac%e2%b0%8f%ef%bc%b2%f0%9d%90%96+-+Lu%2c+uppercase+letters+%28some%29%0d%0a%c7%85%c7%88%c7%8b%c7%b2%e1%be%88%e1%be%89%e1%be%8a%e1%be%8b%e1%be%8c%e1%be%8d%e1%be%8e%e1%be%8f%e1%be%98%e1%be%99%e1%be%9a%e1%be%9b%e1%be%9c%e1%be%9d%e1%be%9e%e1%be%9f%e1%be%a8%e1%be%a9%e1%be%aa%e1%be%ab%e1%be%ac%e1%be%ad%e1%be%ae%e1%be%af%e1%be%bc%e1%bf%8c%e1%bf%bc+-+Lt%2c+titlecase+letters+%28all%29%0d%0a%c7%83%c2%ba%e0%a4%8c%df%a9%d7%94%d7%b2%d8%a8%dc%a2+++-+Lo%2c+other+letters+%28some%29%0d%0a%ca%b0%ca%b7%cb%87%cb%a3%df%b4%df%b5%e0%bb%86%e1%b1%bd%e1%b5%82%e1%b5%92%e1%b5%9d%e1%b6%a3%e2%82%90%e3%80%b1%ea%80%95%ea%9c%9b%ef%bd%b0+-+Lm%2c+Modifier+letters+%28some%29%0d%0ae%d2%87c%cd%a2a%cc%a8+Mn%2c+nonspacing+mark+%28some%29%0d%0a09%d9%a1%d9%a8%df%81%df%88%e0%a9%ae%e0%ad%aa%e0%af%a8%e0%af%ab%e0%b5%ab%e0%b9%95%e0%bc%a5%e1%9f%a8%e1%a7%95%e1%b1%95%ef%bc%95+Nd%2c+decimal+digit+number+%28some%29%0d%0a_%e2%80%bf%e2%81%80%e2%81%94%ef%b8%b3%ef%b8%b4%ef%b9%8d%ef%b9%8e%ef%b9%8f%ef%bc%bf+-+Pc%2c+connector+punctuation&o=m
  [4]: https://i.stack.imgur.com/Yft8i.png
  [5]: https://i.stack.imgur.com/HAQjP.png

## Non-digits matching (negated character class)
    [^0-9]

This will match all characters that are not ASCII digits.

If Unicode digits are also to be negated, the following expression can be used, depending on your flavor/language settings:

    [^\d]

This can be shortened to:

    \D

You may need to enable Unicode character properties support explicitly by using the `u` modifier or programmatically in some languages, but this may be non-obvious. To convey the intent explicitly, the following construct can be used (when support is available):

    \P{N}
    
Which *by definition* means: any character which is not a numeric character in any script. In a negated character range, you may use:

    [^\p{N}]

In the following sentences:

> 1. Hi, what's up?
>
> 2. I can't wait for 2017!!!

The following characters will be matched:
> 1. `,`, ` `, `'`, `?`, the end of line character and all letters (lowercase and uppercase).
>
> 2. `'`, ` `, `!`, the end of line character and all letters (lowercase and uppercase).



## POSIX Character classes
POSIX character classes are predefined sequences for a certain set of characters.

| Character class | Description |
| ------ | ------ |
| `[:alpha:]` | Alphabetic characters |
| `[:alnum:]` | Alphabetic characters and digits |
| `[:digit:]` | Digits |
| `[:xdigit:]` | Hexadecimal digits |
| `[:blank:]` | Space and Tab |
| `[:cntrl:]` | Control characters |
| `[:graph:]` | Visible characters (anything except spaces and control characters) |
| `[:print:]` | Visible characters and spaces |
| `[:lower:]` | Lowercase letters |
| `[:upper:]` | Uppercase letters |
| `[:punct:]` | Punctuation and symbols |
| `[:space:]` | All whitespace characters, including line breaks |


Additional character classes my be available depending on the implementation and/or locale.

| Character class | Description |
| ------ | ------ |
| `[:<:]` | Beginning of word |
| `[:>:]` | End of word |
| `[:ascii:]` | ASCII Characters |
| `[:word:]` | Letters, digits and underscore. Equivalent to `\w` |

To use the inside a bracket sequence (aka. character class), you should also include the square brackets. Example:

    [[:alpha:]]

This will match one alphabetic character.


    [[:digit:]-]{2}

This will match 2 characters, that are either digits or `-`. The following will match:

 - `--`
 - `11`
 - `-2`
 - `3-`


More information is available on: [Regular-expressions.info][1]


  [1]: http://www.regular-expressions.info/posixbrackets.html

