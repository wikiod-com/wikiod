---
title: "Split windows"
slug: "split-windows"
draft: false
images: []
weight: 9863
type: docs
toc: true
---

## Syntax
 - `:split <file>`
 - `:vsplit <file>`
 - `:sp` <- shorthand for split
 - `:vsp` <- shorthand for vsplit

When called from the command line, multiple files can be provided in the argument and vim will create one split for each file. When called from ex mode, only one file can be opened per invocation of the command.

## Opening multiple files in splits from the command line
### Horizontally

    vim -o file1.txt file2.txt

### Vertically

    vim -O file1.txt file2.txt

You may optionally specify the number of splits to open. The following example opens two horizontal splits and loads `file3.txt` in a buffer:

    vim -o2 file1.txt file2.txt file3.txt

## Opening a new split window
You can open a new split within Vim with the following commands, in *normal* mode:

Horizontally:

    :split <file name>
    :new

Vertically:

    :vsplit <file name> 
    :vnew 

**split** will open the file in a new split at the top or left of your screen (or current split.) `:sp` and `:vs` are convenient shortcuts.

**new** will open an empty split 

## Changing the size of a split or vsplit
You may sometimes want to change the size of a split or vsplit.

To change the size of the currently active split, use `:resize <new size>`. `:resize 30` for example would make the split 30 lines tall.

To change the size of the currently active vsplit, use `:vertical resize <new size>`. `:vertical resize 80` for example would make the vsplit 80 characters wide.

Shortcuts
==========

* <kbd>Ctrl + w</kbd> and <kbd>+</kbd> increase the size of the splited window
* <kbd>Ctrl + w</kbd> and <kbd>-</kbd> decrease the size of the splited window
* <kbd>Ctrl + w</kbd> and <kbd>=</kbd> set an equal size to the splited windows

## Close all splits but the current one
Normal mode  
<kbd>Ctrl-w</kbd><kbd>o</kbd>

Ex mode

    :only

or short

    :on

## Move between splits
To move to split on left, use `<C-w><C-h>`  
To move to split below, use `<C-w><C-j>`  
To move to split on right, use `<C-w><C-k>`  
To move to split above, use `<C-w><C-l>`  

## Sane split opening
It's a better experience to open split below and on right

set it using

    set splitbelow
    set splitright

## Managing Open Split Windows (Keyboard Shortcuts)
After you have opened a split window in vim (as demonstrated by many examples under this tag) then you will likely want to control windows quickly. Here is how to control split windows using keyboard shortcuts. 

Move to split Above/Below:
* <kbd>Ctrl + w</kbd> and <kbd>k</kbd> 
* <kbd>Ctrl + w</kbd> and <kbd>j</kbd>

Move to split Left/Right:
* <kbd>Ctrl + w</kbd> and <kbd>h</kbd> 
* <kbd>Ctrl + w</kbd> and <kbd>l</kbd>

Move to split Above/Below (wrap): 
* <kbd>Ctrl + w</kbd> and <kbd>w</kbd> 

Create new empty window:
* <kbd>Ctrl + w</kbd> and <kbd>n</kbd> -or- :new

Create new split horizontal/vertical:

* <kbd>Ctrl</kbd>+<kbd>W</kbd>, <kbd>S</kbd> (upper case)
* <kbd>Ctrl</kbd>+<kbd>W</kbd>, <kbd>v</kbd> (lower case)

Make the currently active split the one on screen: 
* <kbd>Ctrl + w</kbd> and <kbd>o</kbd> -or- :on




