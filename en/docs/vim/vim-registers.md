---
title: "Vim Registers"
slug: "vim-registers"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Parameters
| Functionality | Registers |
|-----------|-------|
|default register  | `""`  |
| history registers | `"[1-9]`|
| yank register| `"0` |
| named registers | `"[a-z]`, `"[A-Z]` same as `"[a-z]` but appends |
| recall current search pattern | `"/` |
| small deletes (diw, cit, ...)   | `"-`|
|expression registers for simple math | `"=` |
|black hole register to eliminate large chunks of deleted text from mem| `"_`|
|last command | `":`|
|last inserted text| `". `|
|filename| `"%`|
|clipboard| `"*`|
|selected text| `"+`|
|dropped text| `"~`|




## Paste the filename while in insert mode using the filename register
In Insert mode, press `<C-r>` and then `%` to insert the filename.  

This technique is applicable to all registers.

For e.g. if in insert mode, you want to paste the current search pattern, you can type `<C-r>` and then `/`.

## Delete a range of lines into a named register
In Normal, type the following to delete a range of lines into a named register

    :10,20d a

This will delete lines 10,20 in register `"a`.
We can verify this by typing

    :reg

This will show the text that was delete in register `"a`.

To paste the contents in `"a`, just type

    "ap 


    

## Copy/paste between Vim and system clipboard
Use the quotestar register to copy/paste between Vim and system clipboard

`"*yy` copies the current line into the system clipboard

`"*p`    pastes the content of the system clipboard into Vim

## Append to a register
Yank all lines containing TODO into a register by using append operation

    :global/TODO/yank A

Here, we are searching for a `TODO` keyword globally, yanking all lines into register `a` (`A` register appends all lines to `a` register). 

NOTE: It is in general a good practice to clear a register before performing the append operation.

To clear a register, in the normal mode, type `qaq`. Confirm that the `a` register is empty by typing `:reg` and observing that `a` register is empty.

