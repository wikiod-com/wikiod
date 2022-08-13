---
title: "global"
slug: "global"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Syntax
- `:[<range>]g[lobal]/{<pattern>}/[<command>]`
- `:[<range>]g[lobal]!/{<pattern>}/[<command>]` (inverted)
- `:[<range>]v[global]/{<pattern>}/[<command>]` (inverted)


Vim's "global" command is used to apply an ex command to every line where a regex matches.

## Basic usage of the Global Command


    :g/Hello/d

Will delete every line containing the text "Hello". **Important note**: This is not the normal mode command `d`, this is the ex command `:d`.

You can use the global command to apply normal mode keystrokes instead of ex commands by prepending `normal` or `norm` to the command. For example:

    :g/Hello/norm dw

Will delete the first word from every line that contains the text "Hello".

The global command also supports visual mode [and ranges](https://www.wikiod.com/vim/command-line-ranges).


## Yank every line matching a pattern
The command

```
:g/apples/y A
``` 

will yank all lines containing the word *apples* into the `a` register, which can be pasted with `"ap`. Any regular expression can be used.

Note the space before the `A`, and the capitalization of the register letter. If a capital letter is used as the yank register, matches will be *appended* to that register. If a lowercase letter is used, only the *last* match will be placed in that register.


## Move/collect lines containing key information
a simple yet very useful command:  

    :g/ending/m$
moves lines containing `ending` to the end of the buffer.

`m` means move  
`$` means end of buffer, while `0` means beginning of buffer.

