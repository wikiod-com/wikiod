---
title: "Useful configurations that can be put in .vimrc"
slug: "useful-configurations-that-can-be-put-in-vimrc"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - set mouse=a
 - set wrap
 - nmap j gj
 - nmap k gk

## Move up/down displayed lines when wrapping
Usually, `J` and `K` move up and down file lines. But when you have wrapping on, you may want them to move up and down the **displayed** lines instead.

    set wrap " if you haven't already set it
    nmap j gj
    nmap k gk

## Enable Mouse Interaction
    set mouse=a

This will enable mouse interaction in the `vim` editor. The mouse can 

* change the current cursor's position
* select text

## Configure the default register to be used as system clipboard
    set clipboard=unnamed

This makes it possible to copy/paste between Vim and the system clipboard without specifying any special register.

`yy` yanks the current line into the system clipboard

`p`    pastes the content of the system clipboard into Vim

This only works if your Vim installation has clipboard support. Run the following command in the terminal to check if the clipboard option is available: `vim --version | grep
clipboard`



