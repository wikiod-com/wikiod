---
title: "Inserting text"
slug: "inserting-text"
draft: false
images: []
weight: 9849
type: docs
toc: true
---

## Running normal commands from insert mode
While in insert mode, press `<C-o>` to temporarily leave insert mode and execute a one-off normal command. 

## Example

`<C-o>2w` jumps to the second word to the left and returns to insert mode.

*Note: Repeating with `.` will only repeat the actions from returning to insert mode*

This allows for some useful mappings, e.g.

    inoremap <C-f> <Right>
    inoremap <C-b> <Left>
    inoremap <C-a> <C-o>^
    inoremap <C-e> <C-o>$

Now ctrl+a will put the cursor to the beginning of the line and ctrl+e - to the end of line. These mappings are used by default in `readline`, so might be useful for people who want consistency.

## Different ways to get into insert mode
Command     | Description
------------|------------
`a`         | Append text following current cursor position
`A`         | Append text at the end of current line
`i`         | Insert text before the current cursor position
`I`         | Insert text before first non-blank character of current line
`gI`        | Insert text in first column of cursor line
`gi`        | Insert text at same position where it was left last time in Insert mode
`O`         | Open up a new line above the current line and add text there (CAPITAL `O`)
`o`         | Open up a new line below the current line and add text there (lowercase `o`)
`s` or `cl` | Delete character under the cursor and switch to insert mode
`S` or `cc` | Delete entire line and switch to Insert mode
`C`         | Delete from the cursor position to the end of the line and start insert mode
`c{motion}` | Delete `{motion}` and start insert mode (see https://www.wikiod.com/vim/movement#Basic Motion\)

## Insert mode shortcuts
Command           | Description
------------------|------------
`<C-w>`           | Delete word before cursor
`<C-t>`           | Indent current line with by one `shiftwidth`
`<C-d>`           | Unindent current line with by one `shiftwidth`
`<C-f>`           | reindent the line, (move cursor to auto indent position)
`<C-a>`           | Insert previously inserted text
`<C-e>`           | Insert the character below
`<C-h>`           | Delete one character backward
`<C-y>`           | Insert the character above
`<C-r>{register}` | Insert the content of `{register}`
`<C-o>{normal mode command}` | execute `{normal mode command}` without leaving insert mode
`<C-n>`            | Next autocomplete option for the current word
`<C-p>`            | Previous autocomplete option for the current word
`<C-v>`            | Insert a character by its ASCII value in decimal
`<C-v>x`           | Insert a character by its ASCII value in hexadecimal
`<C-v>u`           | Insert a character by its unicode value in hexadecimal
`<C-k>`            | Enter a digraph



## Leaving insert mode
Command | Description
--------|------------
`<Esc>` | Leaves insert mode, triggers autocommands and abbreviations
`<C-[>` | Exact synonymous of `<Esc>`
`<C-c>` | Leaves insert mode, doesn't trigger autocommands

Some people like to use a relatively uncommon pair of characters like `jk` as shortcut for `<Esc>` or `<C-[>` which can be hard to reach on some keyboards:

    inoremap jk <Esc>l

## Insert text into multiple lines at once
Press <kbd>Ctrl</kbd> + <kbd>v</kbd> to enter into visual block mode.

Use <kbd>&uarr;</kbd> / <kbd>&darr;</kbd> / <kbd>j</kbd> / <kbd>k</kbd> to select multiple lines.

Press <kbd>Shift</kbd> + <kbd>i</kbd> and start typing what you want.

After you press <kbd>Esc</kbd>, the text will be inserted into all the lines you selected.

Remember that <kbd>Ctrl</kbd>+<kbd>c</kbd> is not 100% equivalent to <kbd>Esc</kbd> and will not work in this situation!

There are slight variations of <kbd>Shift</kbd> + <kbd>i</kbd> that you can press while in visual block mode:

Key        | Description
-----------|------------
`c` or `s` | Delete selected block and enter insert mode
<kbd>C</kbd>        | Delete selected lines (from cursor until end) and enter insert mode
<kbd>R</kbd>        | Delete selected lines and enter insert mode
<kbd>A</kbd>        | Append to selected lines, with the column at the end of the first line

Also note that pressing <kbd>.</kbd> after a visual block operation will repeat that operation where the cursor is!

## Paste text using terminal "paste" command
If you use the paste command from your terminal emulator program, Vim will interpret the stream of characters as if they were typed. That will cause all kind of undesirable effects, particularly bad indendation.

To fix that, from command mode:

    :set paste

Then move on to insert mode, with <kbd>i</kbd>, for example. Notice the mode is now `-- INSERT (paste) --`. Now paste with your terminal emulator command, or with the mouse. When finished go to command mode, with <kbd>Esc</kbd> and run:

    :set nopaste

----------

There is a simpler way, when one wants to paste just once. Put this in your *.vimrc* (or use the plugin *[unimpaired.vim](https://github.com/tpope/vim-unimpaired)*):

    function! s:setup_paste() abort
      set paste
      augroup unimpaired_paste
        autocmd!
        autocmd InsertLeave *
          \ set nopaste |
          \ autocmd! unimpaired_paste
      augroup end
    endfunction
    
    nnoremap <silent> yo :call <SID>setup_paste()<CR>o
    nnoremap <silent> yO :call <SID>setup_paste()<CR>O

Now one can simply press `yo` to paste code under the cursor, and then `<Esc>` to go back to normal/nopaste mode. 


## Pasting from a register while in insert mode
While in insert mode, you can use `<C-r>` to paste from a register, which is specified by the next keystroke. `<C-r>"` for example pastes from the unnamed (`"`) register.

See `:help registers`.

## Advanced Insertion Commands and Shortcuts
Here is a quick reference for advanced insertion, formatting, and filtering commands/shortcuts. 

| **Command/Shortcut**                               | _Result_                                 |
| ---------------------------------------- | ---------------------------------------- |
| <kbd>g + ?</kbd> + *m*                                 | Perform rot13 encoding, on movement *m* |
| *n* + <kbd>ctrl + a</kbd>                             | +*n* to number under cursor |
| *n* + <kbd>ctrl + x</kbd>                                | -*n* to number under cursor |
| 
<kbd>g + q</kbd>+ *m*                             | Format lines of movement *m* to fixed width
 |
| :*r*ce w                                | Center lines in range *r* to width *w* |
| :*r*le i                               |  Left align lines in range *r* with indent *i* |
| :*r*ri *w*                              | Right align lines in range *r* to width *w* |
| !*mc*                             | Filter lines of movement *m* through command *c* |
| *n*!!*c*                               | Filter *n* lines through command *c* |
| :*r*!*c*                              | Filter range *r* lines through command *c* |



## Disable auto-indent to paste code
When pasting text through a terminal emulator, the auto-indent feature *may* destroy the indentation of the pasted text.

For example:

    function () {
        echo 'foo'
        echo 'bar'
        echo 'baz'
    }

will be pasted as:

    function () {
        echo 'foo'
            echo 'bar'
                echo 'baz'
                }
 
In these cases, use the `paste`/`nopaste` option to disable / enable the auto-indent feature:

    :set paste
    :set nopaste

Adding to this, there is a simpler approach to the problem: Add the following line in your .vimrc:

    set pastetoggle=<F3>
And if you want to paste as is from the clipboard. Just press `F3` in `insert` mode, and paste. Press `F3` again to exit from the `paste` mode. 

