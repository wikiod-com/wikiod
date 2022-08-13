---
title: "Enhanced undo and redo with a undodir"
slug: "enhanced-undo-and-redo-with-a-undodir"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Configuring your vimrc to use a undodir
Since vim version 7.3 the feature 'persistent_undo' is supported, which makes it possible do undo/redo changes, even after closing vim or restarting your computer.

It's possible to configure it by adding the following to your vimrc, but first create a directory, where your undofiles should be saved. You can create the file anywhere, but I recommend using the ".vim" directory.

    if has('persistent_undo')         "check if your vim version supports
      set undodir=$HOME/.vim/undo     "directory where the undo files will be stored
      set undofile                    "turn on the feature
    endif

After adding this to your vimrc and sourcing the vimrc again, you can use the feature by using the [basic undo/redo commands][1]


  [1]: https://www.wikiod.com/vim/normal-mode-commands-editing#Basic Undo and Redo

