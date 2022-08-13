---
title: "Autocommands"
slug: "autocommands"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

**Surround `autocmd` commands**

`autocmd` is an additive command, and you probably don't want this behaviour by default.

For example, if you re-source your `.vimrc` a few times while editing it, vim can slow down.

Here's proof:

```
:autocmd BufWritePost * if &diff | diffupdate | endif " update diff after save
:autocmd BufWritePost * if &diff | diffupdate | endif " update diff after save
```

If you now type `:autocmd BufWritePost *`, you'll see **both** lines in the output, not just one. Both get executed.

To avoid this behaviour, surround all your `autocmd`s as follows:

<!-- language: vim -->
```
if has ('autocmd')       " Remain compatible with vi which doesn't have autocmd
  augroup MyDiffUpdate   " A unique name for the group.  DO NOT use the same name twice!
      autocmd!           " Clears the old autocommands for this group name
      autocmd BufWritePost * if &diff | diffupdate | endif   " Update diff after save
      " ... etc ...
  augroup END
endif
```

## Automatically source .vimrc after saving
Add this to your `$MYVIMRC`:

    " Source vim configuration file whenever it is saved
    if has ('autocmd')          " Remain compatible with earlier versions
     augroup Reload_Vimrc       " Group name.  Always use a unique name!
        autocmd!                " Clear any preexisting autocommands from this group
        autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
        autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
      augroup END
    endif " has autocmd

**Features:**

 - `echom` tells the user what has happened (and also logs to `:messages`).
 - `$MYVIMRC` and `$MYGVIMRC` handle platform-specific names for the configuration files,
 - and only match the actual configuration files (ignoring copies in other directories, or a `fugitive://` diff)
 - `has()` will prevent an error if using incompatible versions, such as `vim-tiny`.
 - `autocmd!` avoids buildup of multiple identical autocommands if this file is sourced again.  (It clears all commands in the named group, so the group name is important.)

## Refresh vimdiff views if a file is saved in another window
    :autocmd BufWritePost * if &diff | diffupdate | endif 

