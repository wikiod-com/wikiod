---
title: "Escaping special characters"
slug: "escaping-special-characters"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

In all `cmd.exe` and `DOS` version, some characters are reserved for specific usage(e.g. command redirection). This topic will talk about how to use the special characters without issues.

## Escape using caret(^)
Most special characters can be escaped using the caret(`^`). Take a look at the following example.

    echo > Hi
    echo ^> Hi 

This first command would not output `> Hi` because `>` is a special character, which means redirect output to a file. In this case, the file is named "Hi"

However in the second command, `> Hi` would be outputted without any issue because the caret(`^`) tells the `>` to stop functioning as "redirect output to file" command, now `>` is just a normal character.

---

Here's a list of special characters that can be escaped(taken, and edited from Rob van der Woude's page)

| Character | Escaped Result | Remarks                                  |
| ------    | ------         | -----------------------------------------| 
|  ^        | ^^             |                                          |
|  &        | ^&             |                                          |
|  <        | ^<             |                                          |
| \>        | ^>             |                                          |
|  \|       | ^\|            |                                          |
|  \        |  ^\            |                                          |
| !         |  ^^!           | Only required when DelayedExpansion is on|

# Escaping the caret

Carets can be stacked up to the escape other carets, consider the following example.

| Input         | Output  |
|-------        |---------|
|    ^&         |   &     |
|  ^**^**^&     |  ^&     |
|^**^**^**^**^& | ^^&     |

Note: The carets in bold form are escaped.

---

# Security issue

A bit off topic here, but this is very important! An unwanted caret escape at the end of the file could cause a memory leak!

    any-invalid-command-you-like-here ^

This command would leak all the memory, **rendering the system completely unusable**! See [here][1] for more information.


  [1]: https://stackoverflow.com/questions/15466298/simple-caret-at-end-of-windows-batch-file-consumes-all-memory

## FIND and FINDSTR Special Characters
In `find` and `findstr`, there are some special characters that require some caution on it.

---

# FIND

There is only one character that needs escaping - `"` quote. To escape it, simply add another quote next to it. So `"` becomes `""`. Pretty simple.

---

# FINDSTR

`Findstr` comes with plenty of characters to escape, so please be very cautious. Using `\`, we can escape special characters. Here's a list of special characters to escape

| Character | Escaped Result |
| ------ | ------ |
| \   | \\\   |
| [   | \\[   |
| ]   | \\]   |
| "   | \\"   |
| .   | \\.   |
|\*   | \\*   |
| ?   | \\?   |






## FOR /F Special Characters
# FOR /F

In a `FOR /F` statement, some characters needs escaping, here a list(taken and edited from Rob van der Woude's page)

| Character | Escaped Result | Remarks       |
| ------ | ------ | ------        |
| '      | ^'   | Only needed in `FOR /F`'s brackets, unless `usebackq` is specified.
| `      | ^`   | Only needed in `FOR /F`'s brackets, when `usebackq` is specified
| ,      | ^,   | ┒
| ;      | ^;   | ┃
| =      | ^=   | ┣ Must be escaped in `FOR /F`'s brackets, even if it is double-quoted
| (      | ^(   | ┃
| )      | ^)   | ┙




## Extra Special Characters
Here is a list of other special character(s), that require(s)/may need escaping, but not mentioned above.

| Character | Escaped Result | Remarks |
| ------ | ------ | -------------------|
| %      | %%     |
| [LF]   | ^[LF]  | This trick is metioned by Mark Stang in the `alt.msdos.batch` news group.

## Escaping through the pipeline
When there's an expression with a pipe the `cmd` starts two threads on both sides of the pipe and the expression is parsed twice (for each side of the pipe) so carets need to be doubled.

On the left side:

    echo ^^^&|more

On the right side:

    break|echo ^^^&

