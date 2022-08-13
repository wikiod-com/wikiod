---
title: "Indentation"
slug: "indentation"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Indent an entire file using built-in indentention engine
In command mode(Esc) enter `:gg=G` to use Vim's built-in indention engine.

| Command Part| Description |
| ------ | ------ |
| gg | start of file |
| = | indent (when `equalprg` is empty) |
| G | end of file |

You can set `equalprg` in your .vimrc to use a more sophisticated auto-formatting tool.

For example, to use `clang-format` for C/C++ put the following line in your `.vimrc` file:

    autocmd FileType c,cpp setlocal equalprg=clang-format

For other file types, replace `c,cpp` with the filetype you want to format and `clang-format` with your preferred formatting tool for that filetype.

For example:

    " Use xmllint for indenting XML files. Commented out.
    "autocmd FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
    " Tidy gives more formatting options than xmllint
    autocmd FileType xml setlocal equalprg=tidy\ --indent-spaces\ 4\ --indent-attributes\ yes\ --sort-attributes\ alpha\ --drop-empty-paras\ no\ --vertical-space\ yes\ --wrap\ 80\ -i\ -xml\ 2>/dev/null



## Indent or outdent lines
To indent our outdent the current line in [normal mode][1] press the greater than `>` key or the less than `<` twice accordingly.
To do the same on multiple lines just add a number beforehand `6>>`

| Command | Description |
| ------ | ------ |
| `>>` | indent current line |
| `<<`   | outdent current line |
| `6>>`   | indent next 6 lines |

You can also indent using [motions][2]. Here are a few useful examples.

| Command | Description |
| ------ | ------ |
| `>gg` | indent from current line to first line in file|
| `>G`   | indent from current line to last line in file|    
| `>{`   | indent previous paragraph|    
| `>}`   | indent next paragraph|    

In [visual mode][1] by pressing the greater than or less than key just once. Note that this causes an exit from [visual mode][1]. Then you can use `.` to repeat the edit if you need to and `u` to undo.


  [1]: https://www.wikiod.com/vim/modes---insert-normal-visual-ex
  [2]: https://www.wikiod.com/vim/movement#Basic Motion

