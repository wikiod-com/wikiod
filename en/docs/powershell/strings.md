---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- "(Double-quoted) String"

- 'Literal string'

- @"\
Here-string\
"@
- @'\
Literal here-string\
'@

Strings are objects representing text.

## Creating a basic string
String
======
Strings are created by wrapping the text with double quotes. Double-quoted strings can evalute variables and special characters.

    $myString = "Some basic text"
    $mySecondString = "String with a $variable"

To use a double quote inside a string it needs to be escaped using the escape character, backtick (`` ` ``). Single quotes can be used inside a double-quoted string.

    $myString = "A `"double quoted`" string which also has 'single quotes'."


Literal string
==============
Literal strings are strings that doesn't evaluate variables and special characters. It's created using single quotes. 

    $myLiteralString = 'Simple text including special characters (`n) and a $variable-reference'

To use single quotes inside a literal string, use double single quotes or a literal here-string. Double qutoes can be used safely inside a literal string

    $myLiteralString = 'Simple string with ''single quotes'' and "double quotes".'

## Format string
    $hash = @{ city = 'Berlin' }

    $result = 'You should really visit {0}' -f $hash.city
    Write-Host $result #prints "You should really visit Berlin"

Format strings can be used with the `-f` operator or the static `[String]::Format(string format, args)` .NET method.

## Multiline string
There are multiple ways to create a multiline string in PowerShell:

- You can use the special characters for carriage return and/or newline manually or use the `NewLine`-environment variable to insert the systems "newline" value)

      "Hello`r`nWorld"
      "Hello{0}World" -f [environment]::NewLine

- Create a linebreak while defining a string (before closing quote)

      "Hello
      World"

- Using a here-string. *This is the most common technique.*

      @"
      Hello
      World
      "@

## Here-string
Here-strings are very useful when creating multiline strings. One of the biggest benefits compared to other multiline strings are that you can use quotes without having to escape them using a backtick.

Here-string
===========
Here-strings begin with `@"` and a linebreak and end with `"@` on it's own line (**`"@`must be first characters on the line, not even whitespace/tab**).

    @"
    Simple
        Multiline string 
    with "quotes"
    "@

Literal here-string
===================
You could also create a literal here-string by using single quotes, when you don't want any expressions to be expanded just like a normal literal string.

    @'
    The following line won't be expanded
    $(Get-Date)
    because this is a literal here-string
    '@


## Concatenating strings
Using variables in a string
===========================
You can concatenate strings using variables inside a double-quoted string. This does not work with properties.

    $string1 = "Power"
    $string2 = "Shell"
    "Greetings from $string1$string2"

Using the `+` operator
======================
You can also join strings using the `+` operator.

    $string1 = "Greetings from"
    $string2 = "PowerShell"
    $string1 + " " + $string2

This also works with properties of objects.

    "The title of this console is '" + $host.Name + "'"

Using subexpressions
====================
The output/result of a subexpressions `$()` can be used in a string. This is useful when accessing propeties of an object or performing a complex expression. Subexpressions can contain multiple statements separated by semicolon `;`

    "Tomorrow is $((Get-Date).AddDays(1).DayOfWeek)"

## Special characters
When used inside a double-quoted string, the escape character (backtick `` ` ``) reperesents a special character.

    `0    #Null
    `a    #Alert/Beep
    `b    #Backspace
    `f    #Form feed (used for printer output)
    `n    #New line
    `r    #Carriage return
    `t    #Horizontal tab
    `v    #Vertical tab (used for printer output)

Example:

    > "This`tuses`ttab`r`nThis is on a second line"
    This    uses    tab
    This is on a second line

You can also escape special characters with special meanings:

    `#    #Comment-operator
    `$    #Variable operator
    ``    #Escape character
    `'    #Single quote
    `"    #Double quote

