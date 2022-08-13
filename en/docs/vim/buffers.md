---
title: "Buffers"
slug: "buffers"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Managing buffers
You can use buffers to work with multiple files. When you open a file using

    :e path/to/file

it opens in a new buffer (the command means edit the file). New buffer that holds a temporary copy of the file.

You can go to previous buffer with `:bp[rev]` and next buffer with `:bn[ext]`.

You can go to a particular buffer with `b{n}` to go to nth buffer. `b2` goes to second buffer.

Use `:ls` or `:buffers` to list all buffers

## Hidden buffers
Moving away from a buffer with unsaved changes will cause this error:

    E37: No write since last change (add ! to override)

You can disable this by adding `set hidden` to your .vimrc file.  With this option set your changes will persist in the buffer, but will not be saved to disk.

## Switching buffer using part of the filename
To easily select a buffer by filename, you can use:

    :b [part_of_filename]<Tab><Tab><Tab>...<Enter>

The first <kbd>Tab</kbd> will expand the word to a full filename, and subsequent <kbd>Tab</kbd> presses will cycle through the list of possible matches.

When multiple matches are available, you can see a list of matches *before* the word expansion by setting this option:

    :set wildmode=longest:full:list,full

This allows you to refine your word if the list of matches is too long, but it requires an extra <kbd>Tab</kbd> press to perform the expansion.  Add the setting to your `$MYVIMRC` if you want to keep it.

Some people like to kick-start this process with a keymap that first lists the buffers:

    :nnoremap <Leader>b :set nomore <Bar> :ls <Bar> :set more <CR>:b<Space>

That makes it easy to select a buffer by its number:

    :b [buffer_num]

## Quickly switch to previous buffer, or to any buffer by number
`<C-^>` will switch to and from the previous edited file.  On most keyboards `<C-^>` is CTRL-6.

`3<C-^>` will switch to buffer number 3.  This is very quick, but only if you know the buffer number.

You can see the buffer numbers from `:ls` or from a plugin such as [MiniBufExplorer](https://github.com/fholgado/minibufexpl.vim).

