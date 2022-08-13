---
title: "Regex modifiers (flags)"
slug: "regex-modifiers-flags"
draft: false
images: []
weight: 9862
type: docs
toc: true
---

Regular expression patterns are often used with *modifiers* (also called *flags*) that redefine regex behavior. Regex modifiers can be *regular* (e.g. `/abc/i`) and *inline* (or *embedded*) (e.g. `(?i)abc`). The most common modifiers are global, case-insensitive, multiline and dotall modifiers. However, regex flavors differ in the number of supported regex modifiers and their types.


<h1>PCRE Modifiers</h1>

| Modifier           | Inline | Description |
|--------------------|--------|-------------|
|PCRE_CASELESS       | (?i)   | Case insensitive match                       |
|PCRE_MULTILINE      | (?m)   | Multiple line matching                       |
|PCRE_DOTALL         | (?s)   | `.` matches new lines                        |
|PCRE_ANCHORED       | (?A)   | Meta-character `^` matches only at the start |
|PCRE_EXTENDED       | (?x)   | White-spaces are ignored                     |
|PCRE_DOLLAR_ENDONLY | n/a    | Meta-character `$` matches only at the end   |
|PCRE_EXTRA          | (?X)   | Strict escape parsing                        |
|PCRE_UTF8           |        | Handles `UTF-8` characters                   |
|PCRE_UTF16          |        | Handles `UTF-16` characters                  |
|PCRE_UTF32          |        | Handles `UTF-32` characters                  |
|PCRE_UNGREEDY       | (?U)   | Sets the engine to lazy matching             |
|PCRE_NO_AUTO_CAPTURE| (?:)   | Disables auto-capturing groups               |

<h1>Java Modifiers</h1>

|Modifier (`Pattern.###`)| Value | Description                                  |
|------------------------|-------|----------------------------------------------|
|UNIX_LINES              |  1    | Enables [Unix lines][1] mode.                |
|CASE_INSENSITIVE        |  2    | Enables case-insensitive matching.           |
|COMMENTS                |  4    | Permits whitespace and comments in a pattern.|
|MULTILINE               |  8    | Enables multiline mode.                      |
|LITERAL                 |  16   | Enables literal parsing of the pattern.      |
|DOTALL                  |  32   | Enables dotall mode.                         |
|UNICODE_CASE            |  64   | Enables Unicode-aware case folding.          |
|CANON_EQ                |  128  | Enables canonical equivalence.               |
|UNICODE_CHARACTER_CLASS |  256  | Enables the Unicode version of Predefined character classes and POSIX character classes.                                            |

[1]:https://stackoverflow.com/questions/16064527/pattern-unix-lines-in-regex-with-java

## VERBOSE / COMMENT / IgnorePatternWhitespace modifier
The modifier that allows using whitespace inside some parts of the pattern to format it for better readability and to allow comments starting with `#`:

    /(?x)^          # start of string
      (?=\D*\d)     # the string should contain at least 1 digit 
      (?!\d+$)      # the string cannot consist of digits only
      \#            # the string starts with a hash symbol
      [a-zA-Z0-9]+ # the string should have 1 or more alphanumeric symbols
      $             # end of string
    /

Example of a string: `#word1here`. Note the `#` symbol is escaped to denote a literal `#` that is part of a pattern.

Unescaped white space in the regular expression pattern is ignored, escape it to make it a part of the pattern.

Usually, the whitespace inside character classes (`[...]`) is treated as a literal whitespace, except in Java.

Also, it is worth mentioning that in PCRE, .NET, Python, Ruby Oniguruma, ICU, Boost regex flavors one can use `(?#:...)` comments inside the regex pattern.

## DOTALL modifier
A regex pattern where a DOTALL modifier (in most regex flavors expressed with `s`) changes the behavior of `.` enabling it to match a newline (LF) symbol:

    /cat (.*?) dog/s

This Perl-style regex will match a string like `"cat fled from\na dog"` capturing `"fled from\na"` into Group 1.

An inline version: `(?s)` (e.g. `(?s)cat (.*?) dog`)

**Note**: In Ruby, the DOTALL modifier equivalent is `m`, [`Regexp::MULTILINE` modifier](http://ruby-doc.org/core-2.1.1/Regexp.html#class-Regexp-label-Character+Classes) (e.g. `/a.*b/m`).

**Note**: JavaScript does not provide a DOTALL modifier, so a `.` can never be allowed to match a newline character. In order to achieve the same effect, a workaround is necessary, e. g. substituting all the `.`s with a catch-all character class like `[\S\s]`, or a *not nothing* character class `[^]` (however, this construct will be treated as an error by all other engines, and is thus not portable).

## UNICODE modifier
The UNICODE modifier, usually expressed as `u` (PHP, Python) or `U` (Java), makes the regex engine treat the pattern and the input string as Unicode strings and patterns, make the pattern shorthand classes like `\w`, `\d`, `\s`, etc. Unicode-aware.

    /\A\p{L}+\z/u

is a PHP regex to match strings that consist of 1 or more Unicode letters. See the [regex demo](https://regex101.com/r/dX1cR2/1).

Note that in [**PHP**, the `/u` modifier][1] enables the PCRE engine to handle strings as UTF8 strings (by turning on `PCRE_UTF8` verb) and make the shorthand character classes in the pattern Unicode aware (by enabling `PCRE_UCP` verb, see more at [*pcre.org*](http://www.pcre.org/pcre.txt)).

> **Pattern and subject strings are treated as UTF-8.** This modifier is available from PHP 4.1.0 or greater on Unix and from PHP 4.2.3 on win32. UTF-8 validity of the pattern and the subject is checked since PHP 4.3.5. An invalid subject will cause the preg_* function to match nothing; an invalid pattern will trigger an error of level E_WARNING. Five and six octet UTF-8 sequences are regarded as invalid since PHP 5.3.4 (resp. PCRE 7.3 2007-08-28); formerly those have been regarded as valid UTF-8.

In Python 2.x, the [`re.UNICODE`][2] only affects the pattern itself: *Make `\w`, `\W`, `\b`, `\B`, `\d`, `\D`, `\s` and `\S` dependent on the Unicode character properties database.*

An inline version: `(?u)` in Python, `(?U)` in Java. For example:

    print(re.findall(ur"(?u)\w+", u"Dąb")) # [u'D\u0105b']
    print(re.findall(r"\w+", u"Dąb"))      # [u'D', u'b']

    System.out.println("Dąb".matches("(?U)\\w+")); // true
    System.out.println("Dąb".matches("\\w+"));     // false


  [1]: http://php.net/manual/en/reference.pcre.pattern.modifiers.php
  [2]: https://docs.python.org/2/library/re.html#re.UNICODE

## MULTILINE modifier
Another example is a MULTILINE modifier (usually expressed with `m` flag (not in Oniguruma (e.g. Ruby) that uses `m` to denote a DOTALL modifier)) that makes `^` and `$` anchors match the start/end of a *line*, not the start/end of the whole string.

    /^My Line \d+$/gm

will find all *lines* that start with `My Line`, then contain a space and 1+ digits up to the line end.

An inline version: `(?m)` (e.g. `(?m)^My Line \d+$`)

**NOTE**: In Oniguruma (e.g. in Ruby), and also in almost any text editors supporting regexps, the `^` and `$` anchors denote *line* start/end positions *by default*. You need to use `\A` to define the whole document/string start and `\z` to denote the document/string end. The difference between the `\Z` and `\z` is that the former can match before the final newline (LF) symbol at the end of the string (e.g. `/\Astring\Z/` will find a match in `"string\n"`) (except Python, where `\Z` behavior is equal to `\z` and `\z` anchor is not supported).

## IGNORE CASE modifier
The common modifier to ignore case is `i`:

    /fog/i

will match `Fog`, `foG`, etc.

The inline version of the modifier looks like `(?i)`.

Notes:

In Java, by default, [*case-insensitive matching assumes that only characters in the US-ASCII charset are being matched. *Unicode-aware case-insensitive matching* can be enabled by specifying the `UNICODE_CASE` flag in conjunction with this (`CASE_INSENSITIVE`) flag*.](http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#CASE_INSENSITIVE) (e.g. `Pattern p = Pattern.compile("YOUR_REGEX", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);`). Some more on this can be found at [*Case-Insensitive Matching in Java RegEx*](https://blogs.oracle.com/xuemingshen/entry/case_insensitive_matching_in_java). Also, [`UNICODE_CHARACTER_CLASS`][1] can be used to make matching Unicode aware.


  [1]: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html#UNICODE_CHARACTER_CLASS

## Explicit Capture modifier
This is a .NET regex specific modifier expressed with `n`. When used, unnamed groups (like `(\d+)`) are not captured. Only valid captures are explicitly named groups (e.g. `(?<name> subexpression)`). 

    (?n)(\d+)-(\w+)-(?<id>\w+)

will match the whole `123-1_abc-00098`, but `(\d+)` and `(\w+)` won't create groups in the resulting match object. The only group will be `${id}`. See [demo](http://regexstorm.net/tester?p=(%3fn)(%5cd%2b)-(%5cw%2b)-(%3f%3cid%3e%5cw%2b)&i=123-1_abc-00098&o=ixcs).

## PCRE_DOLLAR_ENDONLY modifier
The PCRE-compliant *PCRE_DOLLAR_ENDONLY* modifier that makes the `$` anchor match at the *very end of the string* (excluding the position before the final newline in the string).

    /^\d+$/D

is equal to
 
    /^\d+\z/

and matches a whole string that consists of 1 or more digits and will not match `"123\n"`, but will match `"123"`.

## PCRE_UNGREEDY modifier
The PCRE-compliant PCRE_UNGREEDY flag expressed with `/U`. It switches greediness inside a pattern: `/a.*?b/U` = `/a.*b/` and vice versa.

## PCRE_INFO_JCHANGED modifier
One more PCRE modifier that allows the use of duplicate named groups. 

**NOTE**: only *inline* version is supported - `(?J)`, and must be placed at the start of the pattern.

If you use

    /(?J)\w+-(?:new-(?<val>\w+)|\d+-empty-(?<val>[^-]+)-collection)/

the "val" group values will be never empty (will always be set). A similar effect can be achieved with branch reset though.

## PCRE_EXTRA modifier
A PCRE modifier that causes an error if any backslash in a pattern is followed by a letter that has no special meaning. By default, a backslash followed by a letter with no special meaning is treated as a literal.

E.g.

    /big\y/

will match `bigy`, but

    /big\y/X

will throw an exception.

Inline version: `(?X)`

## PCRE_ANCHORED modifier
Another PCRE-compliant modifier expressed with `/A`  modifier. [If this modifier is set, the pattern is forced to be "anchored", that is, it is constrained to match only at the start of the string which is being searched (the "subject string"). This effect can also be achieved by appropriate constructs in the pattern itself, which is the only way to do it in Perl.](http://php.net/manual/en/reference.pcre.pattern.modifiers.php)

    /man/A

is the same as

    /^man/



