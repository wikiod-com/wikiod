---
title: "Differences between Neovim and Vim"
slug: "differences-between-neovim-and-vim"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Configuration Files
In Vim, your configuration file is `~/.vimrc`, with further configuration files in `~/.vim`.

In Neovim, configuration files are located in `~/.config/nvim`. There is also no `~/.nvimrc` file. Instead, use `~/.config/nvim/init.vim`.

You can import your Vim configuration directly into Neovim using this Unix command:

    ln -s ~/.vimrc ~/.config/nvim/init.vim


