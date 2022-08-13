---
title: "Plugins"
slug: "plugins"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## NERD Tree
NERD TREE is a plugin by scrooloose that allows you to explore the file system while using vim.  You can open files and directories via a tree system that you can manipulate with the keyboard or the mouse.

Add this to your .vimrc to start NERDTree automatically when vim starts up:

    autocmd vimenter * NERDTree

To automatically close NERDTree if it's the only window left add this to your .vimrc:

    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

It's recommended to map a key combination to the NERDTreeToggle command.  Add this to your .vimrc (this example uses Ctrl + N)

    map <C-n> :NERDTreeToggle<CR>

Full details and installation instructions can be view on their [Github](https://github.com/scrooloose/nerdtree).

## Fugitive Vim
Fugitive Vim is a plugin by Tim Pope that provides access to git commands that you can execute without leaving vim.  

Some common commands include:  

`:Gedit` - edit a file in the index and write it to stage the the changes  
`:Gstatus` - equivalent of `git status`  
`:Gblame` - brings up vertical split of output from `git blame`  
`:Gmove` - for git mv  
`:Gremove` - for git rm  
`:Git` - run any command  

It also adds items to the `statusline` like indicating the current branch.

Please see their [GitHub](https://github.com/tpope/vim-fugitive) for more details and installation instructions.

