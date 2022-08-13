---
title: "Getting started with Regular Expressions"
slug: "getting-started-with-regular-expressions"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Character Guide
Note that some syntax elements have different behavior depending on the expression.

Syntax      | Description
------      | ------
`?`         | Match the preceding character or subexpression 0 or 1 times. Also used for non-capturing groups, and named capturing groups.
`*`         | Match the preceding character or subexpression 0 or more times.
`+`         | Match the preceding character or subexpression 1 or more times.
`{n}`       | Match the preceding character or subexpression exactly *n* times.
`{min,}`    | Match the preceding character or subexpression *min* or more times.
`{,max}`    | Match the preceding character or subexpression *max* or fewer times.
`{min,max}` | Match the preceding character or subexpression at least *min* times but no more than `max` times.
| `-` | When included between square brackets indicates `to`; e.g. [3-6] matches characters 3, 4, 5, or 6. |
| `^` | Start of string (or start of line if the multiline `/m` option is specified), or negates a list of options (i.e. if within square brackets `[]`) |
| `$` | End of string (or end of a line if the multiline `/m` option is specified). |
| `(`...`)` | Groups subexpressions, captures matching content in special variables (`\1`, `\2`, etc.) that can be used later within the same regex, for example `(\w+)\s\1\s` matches word repetition |
| `(?<name>`...`)` | Groups subexpressions, and captures them in a named group |
| `(?:`...`)` | Groups subexpressions without capturing |
| `.` | Matches any character except line breaks (`\n`, and usually `\r`). |
| `[`...`]` | Any character between these brackets should be matched once.  NB: `^` following the open bracket negates this effect.  `-` occurring inside the brackets allows a range of values to be specified (unless it's the first or last character, in which case it just represents a regular dash). |
| `\` | Escapes the following character. Also used in meta sequences - regex tokens with special meaning. |
| `\$` | dollar (i.e. an escaped special character) |
| `\(` | open parenthesis (i.e. an escaped special character) |
| `\)` | close parenthesis (i.e. an escaped special character) |
| `\*` | asterisk (i.e. an escaped special character) |
| `\.` | dot (i.e. an escaped special character) |
| `\?` | question mark (i.e. an escaped special character) |
| `\[` | left (open) square bracket (i.e. an escaped special character) |
| `\\` | backslash (i.e. an escaped special character) |
| `\]` | right (close) square bracket (i.e. an escaped special character) |
| `\^` | caret (i.e. an escaped special character) |
| `\{` | left (open) curly bracket / brace (i.e. an escaped special character) |
| `\|` | pipe (i.e. an escaped special character) |
| `\}` | right (close) curly bracket / brace (i.e. an escaped special character) |
| `\+` | plus (i.e. an escaped special character) |
| `\A` | start of a string |
| `\Z` | end of a string |
| `\z` | absolute of a string |
| `\b` | word (alphanumeric sequence) boundary|
| `\1`,`\2`, etc. | back-references to previously matched subexpressions, grouped by `()`, `\1` means the first match, `\2` means second match etc. |
| `[\b]` | backspace - when `\b` is inside a character class ( `[]` )matches backspace |
| `\B` | negated `\b` - matches at any position between two-word characters as well as at any position between two non-word characters|
| `\D` | non-digit  |
| `\d` | digit |
| `\e` | escape |
| `\f` | form feed |
| `\n` | line feed |
| `\r` | carriage return |
| `\S` | non-white-space |
| `\s` | white-space |
| `\t` | tab |
| `\v` | vertical tab |
| `\W` | non-word |
| `\w` | word (i.e. alphanumeric character) |
| `{`...`}` | named character set |
| <code>&#124;</code> | or; i.e. delineates the prior and preceding options. |

