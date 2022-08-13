---
title: "Regex"
slug: "regex"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Referencing Capture Groups
# Backreferences #

["Backreferences"][2] are references in a search regex to capture groups in the same search regex.  The "search regex" is the regex used in the "Find" field of the Find/Replace dialog box.  Here is the most common backreference syntax:

    Absolute:  (group one)...\1

    Relative:  (group one)(group two)...\-1

    Named:     (?<name>group)...\k<name>

# Substitutions

"Substitutions" are references in a replacement regex to capture groups in the associated search regex.  The "replacement regex" is the regex used in the "Replace" field of the Find/Replace dialog box.  Here is the most common substitution syntax:

    Absolute:  $1

    Named:     $+{name}

# More Than 9 Groups #

The formats `\1`, `\-1`, and `$n` are limited single-digit numbers.  In order to positionally reference a capture group greater than nine, use the following syntax instead:

    Backreference:  (group one)...(group ten)...\g{10}

    Substitution:   ${10}




  [1]: http://www.regular-expressions.info/brackets.html
  [2]: http://www.regular-expressions.info/backref.html

## Capture Groups
A regex in Notepad++ may have as many capture groups as desired.

    (one)(two)(three)...(nine)(more than nine groups!)...

[**Anonymous capture groups**][1] use the standard syntax:

    (group)

[**Named capture groups**][2] may use either of following syntax formats:

    (?<name>group)

    (?'name'group)

Anonymous and named capture groups may be mixed in any order:

    (anonymous)(?<name>named)(anonymous)

Capture groups are numbered starting from `1` based on the order of their opening parenthesis', regardless of nesting:

    ((group 2) group 1)

Note that named groups are included in the numbering:

    (group 1)(?<name>group 2)(group 3)


  [1]: http://www.regular-expressions.info/brackets.html
  [2]: http://www.regular-expressions.info/named.html

