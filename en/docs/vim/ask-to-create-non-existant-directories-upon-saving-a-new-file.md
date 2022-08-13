---
title: "Ask to create non-existant directories upon saving a new file"
slug: "ask-to-create-non-existant-directories-upon-saving-a-new-file"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

If you edit a new file: `vim these/directories/dont/exist/newfile`, you won't be able to save the file as the directory `vim` is trying to save into does not exist.

## Prompt to create directories with :w, or sliently create them with :w!
[This code](http://travisjeffery.com/b/2011/11/saving-files-in-nonexistent-directories-with-vim/) will prompt you to create the directory with `:w`, or just do it with `:w!`:

    augroup vimrc-auto-mkdir
      autocmd!
      autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
      function! s:auto_mkdir(dir, force)
        if !isdirectory(a:dir)
              \   && (a:force
              \       || input("'" . a:dir . "' does not exist. Create? [y/N]") =~? '^y\%[es]$')
          call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
        endif
      endfunction
    augroup END



