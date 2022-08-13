---
title: "Configuration"
slug: "configuration"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Tmux configurations goes in a file called `.tmux.conf` in home directory. ie, `~/.tmux.conf`

## Change prefix
The default configuration `<C-b>` is not the easiest to use. It'd be better to use something like <kbd>ctrl</kbd><kbd>a</kbd>. To do so, add this to tmux.conf

    # remap prefix from 'C-b' to 'C-a'
    unbind C-b
    set-option -g prefix C-a
    bind-key C-a send-prefix

