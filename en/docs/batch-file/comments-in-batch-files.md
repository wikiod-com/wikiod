---
title: "Comments in Batch Files"
slug: "comments-in-batch-files"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

Comments are used to show information in a batch script.

## Syntax
- REM
- &REM
- ::
- &::
- Goto :Label
  
          Comments. You can also use |>< ,etc.
 
  :Label
  

## Using Labels as Comments
    ::This is a label that acts as a comment
The double-colon `::` comment shown above is not documented as being a comment command, but it is a special case of a label that acts as a comment.

**Caution**: when labels are used as comments within a bracketed code block or `for` command, the command processor expects every label to be followed by at least one command, 
so when a jump is made to the label it will have something to execute.

The `cmd` shell will try to execute the second line even if it is formatted as a label (and **this causes an error**):

    (
    echo This example will fail
    :: some comment
    )

When working within bracketed code blocks it is definitely safer to use [REM][1] for all comment lines.


  [1]: https://www.wikiod.com/batch-file/comments-in-batch-files#Using REM for Comments

## Using Variables as Comments
It is also possible to use variables as comments. This can be useful to conditionally prevent commands being executed:

    @echo off 
    setlocal
    if /i "%~1"=="update" (set _skip=) Else (set _skip=REM)
    %_skip% copy update.dat 
    %_skip% echo Update applied 
    ... 

When using the above code snippet in a batch file the lines beginning with `%_skip%` are only executed if the batch file is called with `update` as a parameter.


## Block Comments
The batch file format does not have a block comment syntax, but there is an easy workaround for this.

Normally each line of a batch file is read and then executed by the parser, but a 
`goto` statement can be used to jump past a block of plain text (which can be used as a block comment):

    @echo off
    goto :start
    
    A multi-line comment block can go here.
    It can also include special characters such as | >
    
    :start
Since the parser never sees the lines between the `goto :start` statement and `:start` label it can contain arbitrary text (including control characters without the need to escape them) and the parser will not throw an error.


## Using REM for Comments
    REM This is a comment
- `REM` is the official comment command.


## Comment on the code's line
To comment on the same line as the code you can use `&::` or `&rem`. You can also use `&&` or `||` to replace `&`.

Example :

    @echo off
    echo This is a test &::This is a comment
    echo This is another test &rem This is another comment
    pause

**A curiosity**: `SET` command allows *limited* inline comments without `&rem`:

    set "varname=varvalue"    limited inline comment here

Limitations:

- syntax with double quotes `set "varname=varvalue"` or `set "varname="`,
- an inline comment **may not** contain any double quote,
- any `cmd` poisonous characters `| < > &` **must be** properly escaped as `^| ^< ^> ^&`,
- parentheses `( )`  **must be** properly escaped as `^( ^)` within a bracketed code block.

## Batch and WSF Hybrid Comment
    <!-- : Comment

This works with both batch script and WSF. The closing tag(`-->`), only works in WSF.


| Code | Sucessful in both batch and WSF? |
| ------ | ------ |
| `<!--: Comment`    | True    | 
| `<!--: Comment -->`| False - The closing tag only works for WSF |
| `-->`              | False

