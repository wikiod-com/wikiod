---
title: "Get help (using Vim's built-in manual)"
slug: "get-help-using-vims-built-in-manual"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Vim's built-in manual is the authoritative source of information and documentation on every Vim feature, including configurations, built-in functions, and even Vimscript. While not the most beginner-friendly interface, if you know how to look through it, you can find what you need.

Start searching by executing `:help`, `:help [subject]`, or `:help :help`.

## Syntax

 - `:h[elp] [keyword]`

## Parameters
| Parameters | Details |
| --- | --- |
| `keyword` | Configuration, function name, or any other keyword with significance to Vim. Keywords with a leading colon `:` search for Vim commands; e.g `:help :split` yields the window-splitting command, and `:help split` yields the Vimscript function `split()` .

## Getting started / Navigating help files
From anywhere in Vim, execute `:help :help`. This will open a horizontally split window with the manual page for the `:help` command. `:help` by itself will take you to the Table of Contents for the manual itself.

Vim's help files are navigable like regular files (you can search for keywords within a file like normal, with `/`), and additionally they are linked together by tags. Jump to the destination of a tag with `CTRL-]`.

Tags are words surrounded by pipe `|` characters. Versions 7.3 and up 'conceal' those pipe characters (`:help conceal`) and highlight them.

For example, the Table of Contents page shows the following. All of the words highlighted in blue are tags and are surrounded by pipe characters. Typing `CTRL-]` with the cursor on `quickref` will take you to a useful page with a list of tags to useful Vim features. 

[![table-of-contents][1]][1]


  [1]: https://i.stack.imgur.com/rbEmW.png

## Searching the manual
`:help [subject]` attempts to find the "best" match for the argument you supply. The argument "can include wildcards like `*`, `?` and `[a-z]` (any letter).  

You can additionally use Vim's command-line completion with `CTRL+D`:  
`:help spli<Ctrl-D>` will display a list of help topics matching the pattern `spli`, including `split()`, and `:split`.

To search for `Ctrl`-based commands, like `Ctrl-V`, type:  
`:help ^V` with a literal caret character, or even more specifically,  
`:help i_^V` to get help on `Ctrl-V` in insert mode.

As you see, vim has a nomenclature for its help topics. For instance, options are quoted (see `:h 'sw'`), commands start with a colon (see `:h :split`), functions end with empty brackets (see `:h split()`), insert mode mappings start with `i_`, command mode mappings start with `c_`, and so on, except normal mode mappings that have no prefix.

| Search term| Help page|
| ------ | ------ |
| `:help textwidth`   | Configuration for line-length/text-width   |
| `:help normal`   | `:normal`command, to execute normal-mode commands from the command-line   |
| `:help cursor`   | Vimscript command to move the cursor around   |
| `:help buffers`   | Working with buffers; same as `:help windows`   |
| `:help :buffer`   | The `:buffer` command   |
| `:help :vs`   | Vertical splitting   |

