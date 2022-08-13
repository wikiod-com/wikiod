---
title: "Anchor Characters Caret (^)"
slug: "anchor-characters-caret-"
draft: false
images: []
weight: 9771
type: docs
toc: true
---

**Terminology**

The Caret (^) character is also referred to by the following terms:  
- hat
- control
- uparrow
- chevron
- circumflex accent

**Usage**

It has two uses in regular expressions:
- To denote the start of the line
- If used immediately after a square bracket (`[^`) it acts to negate the set of allowed characters (i.e. `[123]` means the character 1, 2, or 3 is allowed, whilst the statement `[^123]` means any character other than 1, 2, or 3 is allowed.

**Character Escaping**

To express a caret without special meaning, it should be escaped by preceding it with a backslash; i.e. `\^`.



## Start of Line
## When multi-line `(?m)` modifier is turned **off**, `^` matches only the input string's beginning:

For the regex

    ^He

The following input strings match:

 - `Hedgehog\nFirst line\nLast line`
 - `Help me, please`
 - `He`

And the following input strings do **not** match:

 - `First line\nHedgehog\nLast line`
 - `IHedgehog`
 - <code>   Hedgehog   </code> (due to white-spaces ` `)

<br>

## When multi-line `(?m)` modifier is turned **on**, `^` matches every line's beginning:

    ^He

The above would match any input string that contains a line beginning with `He`.

Considering `\n` as the new line character, the following lines match:

 - `Hello`
 - `First line\nHedgehog\nLast line` (second line only)
 - `My\nText\nIs\nHere` (last line only)

And the following input strings do **not** match:

 - `Camden Hells Brewery`
 - <code>    Helmet    </code> (due to white-spaces ` `)

<br>

## Matching empty lines using `^`

Another typical use case for caret is matching empty lines (or an empty string if the multi-line modifier is turned off).

In order to match an empty line (multi-line **on**), a caret is used next to a `$` which is another anchor character representing the position at the end of line (https://www.wikiod.com/regex/anchor-characters-dollar- ). Therefore, the following regular expression will match an empty line:

     ^$

## Escaping the caret character
If you need to use the `^` character in a character class (https://www.wikiod.com/regex/character-classes) ), either put it somewhere other than the beginning of the class:

    [12^3]

Or escape the `^` using a backslash `\`:

    [\^123]

If you want to match the caret character itself outside a character class, you need to escape it:

    \^

This prevents the `^` being interpreted as the anchor character representing the beginning of the string/line.

## Comparison start of line anchor and start of string anchor
While many people think that `^` means the start of a string, it [actually means](http://www.regular-expressions.info/anchors.html) start of a line. For an actual start of string anchor use, `\A`.

The string `hello\nworld` (or more clearly)

    hello
    world

Would be matched by the regular expressions `^h`, `^w` and `\Ah` but not by `\Aw`

## Multiline modifier
By default, the caret `^` metacharacter matches the **position** before the first 
character in the string.

Given the string "**charsequence**" applied
against the following patterns: `/^char/` & `/^sequence/`, the engine will try to match as follows:

 - `/^char/`
     - **^** - ` `charsequence 
     - **c** - `c`harsequence
     - **h** - `ch`arsequence
     - **a** - `cha`rsequence
     - **r** - `char`sequence 

    **Match Found**

 - `/^sequence/`
    - **^** - ` `charsequence
    - **s** - ` `charsequence

    **Match not Found**
    
The same behaviour will be applied even if the string contains *line terminators*, such as `\r?\n`.  Only the position at the start of the string will be matched.

For example:


    /^/g

 
> ┊char\r\n  
> \r\n  
> sequence
       
However, if you need to match after every line terminator, you will have to set the **multiline** mode (`//m`, `(?m)`) within your pattern. By doing so, the caret `^` will match "the beginning of each line", which corresponds to the position at the beginning of the string and the positions **immediately after**<sup>1</sup> the line terminators.

<sup><sup>1</sup> In some flavors (Java, PCRE, ...), `^` will not match after the line terminator, if the line terminator is the last in the string.</sup>

For example:

    /^/gm

>┊char\r\n  
>┊\r\n  
>┊sequence

Some of the regular expression engines that support Multiline modifier:

 - [Java][1]

       Pattern pattern = Pattern.compile("(?m)^abc");
       Pattern pattern = Pattern.compile("^abc", Pattern.MULTILINE);

 - [.NET][2]

       var abcRegex = new Regex("(?m)^abc");
       var abdRegex = new Regex("^abc", RegexOptions.Multiline)

 - [PCRE][3]

       /(?m)^abc/
       /^abc/m

 - Python [2][4] & [3][5] (built-in `re` module)

       abc_regex = re.compile("(?m)^abc");
       abc_regex = re.compile("^abc", re.MULTILINE); 


  [1]: https://docs.oracle.com/javase/tutorial/essential/regex/
  [2]: https://msdn.microsoft.com/en-us/library/hs600312(v=vs.110).aspx
  [3]: http://pcre.org/
  [4]: https://docs.python.org/2/library/re.html
  [5]: https://docs.python.org/3/library/re.html

