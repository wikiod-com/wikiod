---
title: "Quoting"
slug: "quoting"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

References
---
- [The POSIX 'Shell Command Language' section on 'Quoting'][posix]

[posix]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_02

## Single-Quotes
Single-quotes are literal strings, and the lack of escape characters means that the only character that can not occur inside of a single-quoted string is a single-quote.

    $ echo '$var \$var \\$var \\\$var'
    $var \$var \\$var \\\$var
    $ echo '"quoted string"'
    "quoted string"
    $ echo 'var=$(echo $var)'
    var=$(echo $var)

## Double-Quotes
Double-quotes preserve all characters other than `"` terminator, `$` expansions, `` ` `` command substitutions, and `\` escapes of any of these characters (and newline removal).  Note that the literal `\` is preserved unless followed by a special character.

General escapes:

    $ printf "\"quoted string\"\\n"
    "quoted string"
    $ printf "\`\`quoted string''\n"
    ``quoted string''
    $ printf "four\\\\nthree\\\ntwo\\none\n"
    four\nthree\ntwo
    one
    $ echo "var=\`echo \$var\`"
    var=`echo $var`
    $ echo "var=\$(echo \$var)"
    var=$(echo $var)

Variable expansion:

    $ var=variable echo "$var \$var \\$var \\\$var"
    variable $var \variable \$var

Command substitution:

    $ var=variable echo "var=`echo $var`"
    var=variable
    $ var=variable echo "var=$(echo $var)"
    var=variable


Removing newlines:

    $ echo "multi\
    > -line"
    multi-line


## Escaping
`\` escapes preserve the following character value, unless the following character is a newline in which case both the `\` and the newline are removed.

Escaping special characters:

    $ echo \"quoted text\"
    "quoted text"
    $ echo \`\`quoted text\'\'
    ``quoted text''
    $ echo 'single-quotes inside of a '\''single-quoted'\'' string'
    single-quotes inside of a 'single-quoted' string
    $ printf format\ with\ %s spaces
    format with spaces
    $ printf %s\\n \$var
    $var

Removing newlines:

    $ echo multi\
    > -line
    multi-line
    

