---
title: "Escaping"
slug: "escaping"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## What characters need to be escaped?
Character escaping is what allows certain characters (reserved by the regex engine for manipulating searches) to be literally searched for and found in the input string. Escaping depends on context, therefore this example does not cover [string][1] or [delimiter][1] escaping.

Backslashes
---
Saying that backslash is the "escape" character is a bit misleading. Backslash escapes and backslash brings; it actually toggles on or off the metacharacter vs. literal status of the character in front of it.

In order to use a literal backslash anywhere in a regex, it must be escaped by another backslash.

Escaping (outside character classes)
---
There are several characters that need to be escaped to be taken literally (at least outside char classes):

- Brackets: `[]`
- Parentheses: `()`
- Curly braces: `{}`
- Operators: `*`, `+`, `?`, `|`
- Anchors: `^`, `$`
- Others: `.`, `\`
 - In order to use a literal `^` at the start or a literal `$` at the end of a regex, the character must be escaped.
 - Some flavors only use `^` and `$` as metacharacters when they are at the start or end of the regex respectively. In those flavors, no additional escaping is necessary. It's usually just best to escape them anyway.



Escaping within Character Classes
---
- It's best practice to escape square brackets (`[` and `]`) when they appear as literals in a char class. Under certain conditions, it's [not required, depending on the flavor](http://codegolf.stackexchange.com/a/47435), but it harms readability.
- The caret, `^`, is  a meta character when put as the first character in a char class: `[^aeiou]`. Anywhere else in the char class, it is just a literal character.
- The dash, `-`, is a meta character, unless it's at the beginning or end of a character class. If the first character in the char class is a caret `^`, then it will be a literal if it is the second character in the char class.

Escaping the Replacement
---
There are also rules for escaping within the replacement, but none of the rules above apply. The only metacharacters are `$` and `\`, at least when `$` can be used to reference capture groups (like `$1` for group 1). To use a literal `$`, escape it: `\$5.00`. Likewise `\`: `C:\\Program Files\\`.

----


BRE Exceptions
---
While ERE (extended regular expressions) mirrors the typical, Perl-style syntax, BRE (basic regular expressions) has significant differences when it comes to escaping:

- There is different shorthand syntax. All of the `\d`, `\s`, `\w` and so on is gone. Instead, it has its own syntax (which POSIX confusingly calls "character classes"), like `[:digit:]`. These constructs must be within a character class.
- There are few metacharacters (`.`, `*`, `^`, `$`) that can be used normally. ALL of the other metacharacters must be escaped differently:

Braces `{}`
- `a{1,2}` matches `a{1,2}`. To match either `a` or `aa`, use `a\{1,2\}`

Parentheses `()`
- `(ab)\1` is invalid, since there is no capture group 1. To fix it and match `abab` use `\(ab\)\1`

Backslash
- Inside char classes (which are called bracket expressions in POSIX), backslash is not a metacharacter (and does not need escaping). `[\d]` matches either `\` or `d`.
- Anywhere else, escape as usual.

Other
- `+` and `?` are literals. If the BRE engine supports them as metacharacters, they must be escaped as `\?` and `\+`.


  [1]: https://www.wikiod.com/regex/escaping#/Delimiters/

## Strings
In most programming languages, in order to have a backslash in a string generated from a string literal, each backslash must be doubled in the string literal. Otherwise, it will be interpreted as an escape for the next character. 

Unfortunately, any backslash required by the regex must be a literal backslash. This is why it becomes necessary to have "escaped escapes" (`\\`) when regexes are generated from string literals.

In addition, quotes (`"` or `'`) in the string literal may need to be escaped, depending on which surround the string literal. In some languages, it is possible to use either style of quotes for a string (choose the most readable one for escaping the entire string literal).

In some languages (e.g.: Java <=7), regexes cannot be expressed directly as literals such as `/\w/`; they must be generated from strings, and normally string literals are used - in this case, `"\\w"`. In these cases, literal characters such as quotes, backslashes, etc. need to be escaped. The easiest way to accomplish this may be by using a tool (like [*RegexPlanet*](http://www.regexplanet.com/advanced/java/index.html)). This specific tool is designed for Java, but it will work for any language with a similar string syntax.

## /Delimiters/
Many languages allow regex to be enclosed or delimited between a couple of specific characters, usually the forward slash `/`.

Delimiters have an impact on escaping: if the delimiter is `/` and the regex needs to look for `/` literals, then the forward slash must be escaped before it can be a literal (`\/`).

Excessive escaping harms readability, so it's important to consider the available options:


Javascript is unique because it allows forward slash as a delimiter, but nothing else (although it does allow [stringified regexes](http://stackoverflow.com/questions/23438047)).


<!-- if version <Perl> [gte 1] -->
Perl, for example, allows almost anything to be a delimiter. Even Arabic characters:

    $str =~ m ش ش

Specific rules are mentioned in [Perl's documentation](http://perldoc.perl.org/perlop.html#Regexp-Quote-Like-Operators).
<!-- end version if -->


[PCRE allows](http://php.net/manual/en/regexp.reference.delimiters.php) two types of delimiters: matched delimiters and bracket-style delimiters. Matched delimiters make use of a single character's pair, while bracket-style delimiters make use of a couple of characters which represents an opening and closing pair.

- Matching delimiters: ``!"#$%&'*+,./:;=?@^_`|~-``
- Bracket-style delimiters: `()`, `{}`, `[]`, `<>`



## Raw String Literals
It's best for readability (and your sanity) to avoid escaping the escapes. That's where raw strings literals come in. (Note that some languages allow delimiters, which are preferred over strings usually. But that's another section.)

They usually work the same way as [this answer describes](http://stackoverflow.com/a/2081708/6083675):

>[A] backslash, `\`, is taken as meaning "just a backslash" (except when it comes right before a quote that would otherwise terminate the literal) -- no "escape sequences" to represent newlines, tabs, backspaces, form-feeds, and so on. 

Not all languages have them, and those that do use varying syntax. C# actually calls them [*verbatim string literals*](https://msdn.microsoft.com/en-us/library/ms228362.aspx#Anchor_3), but it's the same thing.

-----

# Python
<!-- language: lang-py -->
    pattern = r"regex"

<!---->

    pattern = r'regex'


# C++ (11+)
<!-- language: lang-c++ -->

The syntax here is extremely versatile. The only rule is to use a delimiter that does not appear anywhere in the regex. If you do that, no additional escaping is necessary for anything in the string. Note that the parenthesis `()` are not part of the regex:

    pattern = R"delimiter(regex)delimiter";

# VB.NET

Just use a normal string. Backslashes are ALWAYS [literals](http://stackoverflow.com/a/13155483/6083675). 

# C#
<!-- language: lang-c# -->

    pattern = @"regex";

Note that this syntax also [allows](https://msdn.microsoft.com/en-us/library/362314fe.aspx) `""` (two double quotes) as an escaped form of `"`.

