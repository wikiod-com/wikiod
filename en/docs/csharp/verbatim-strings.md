---
title: "Verbatim Strings"
slug: "verbatim-strings"
draft: false
images: []
weight: 9434
type: docs
toc: true
---

## Syntax
- @"verbatim strings are strings whose contents are not escaped, so in this case \n does not represent the newline character but two individual characters: \ and n. Verbatim strings are created prefixing the string contents with the @ character"

- @"To escape quotation marks, ""double quotation marks"" are used."

To concatenate string literals, use the @ symbol at the beginning of each string.

    var combinedString = @"\t means a tab" + @" and \n means a newline";

## Interpolated Verbatim Strings
Verbatim strings can be combined with the new https://www.wikiod.com/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation features found in C#6.

    Console.WriteLine($@"Testing \n 1 2 {5 - 2}
    New line");

**Output:**

> Testing \n 1 2 3  
> New line

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/cWyQE2)

As expected from a verbatim string, the backslashes are ignored as escape characters. And as expected from an interpolated string, any expression inside curly braces is evaluated before being inserted into the string at that position.


## Escaping Double Quotes
Double Quotes inside verbatim strings can be escaped by using 2 sequential double quotes `""` to represent one double quote `"` in the resulting string.

    var str = @"""I don't think so,"" he said.";
    Console.WriteLine(str);

**Output:**
>"I don't think so," he said. 

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/c4OJoq)

## Verbatim strings instruct the compiler to not use character escapes
In a normal string, the backslash character is the escape character, which instructs the compiler to look at the next character(s) to determine the actual character in the string. ([Full list of character escapes][1])

In verbatim strings, there are no character escapes (except for `""` which is turned into a `"`).
To use a verbatim string, just prepend a `@` before the starting quotes.

This verbatim string

    var filename = @"c:\temp\newfile.txt"

**Output:**

>c:\temp\newfile.txt

As opposed to using an ordinary (non-verbatim) string:

    var filename = "c:\temp\newfile.txt"

that will output:

    c:    emp
    ewfile.txt

using character escaping. (The `\t` is replaced with a tab character and the `\n` is replace with a newline.)

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/7kslXQ)






  [1]: https://www.wikiod.com/docs/c%23/39/string-escape-sequences#t=201607172257361795538&a=syntax

## Multiline Strings
    var multiLine = @"This is a 

    multiline paragraph";

**Output:**
>This is a 
>
>multiline paragraph

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/kfOUcH)

Multi-line strings that contain double-quotes can also be escaped just as they were on a single line, because they are verbatim strings.
 
    var multilineWithDoubleQuotes = @"I went to a city named

                            ""San Diego""

                          during summer vacation.";

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/0hwJpf)

*It should be noted that the spaces/tabulations at the start of lines 2 and 3 here are actually present in the value of the variable; check [this question](http://stackoverflow.com/questions/7178136/multiline-formatting-for-verbatim-strings-in-c-sharp-prefix-with) for possible solutions.*


