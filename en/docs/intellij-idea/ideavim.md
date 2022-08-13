---
title: "IdeaVim"
slug: "ideavim"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

IdeaVim is a [plugin for IDEA products](https://plugins.jetbrains.com/plugin/164?pr=idea) that aims in providing Vim functionality in editor views

## Showing line numbers
As of IntelliJ IDEA version `2016.2`, and IdeaVim version `0.46`, IntelliJ's native option for showing line numbers is ineffective. When clicking _Show line numbers_, the line numbers immediately show and disappear.

This problem is caused by a bug in the IdeaVim plugin, which can be resolved by using the Vim command for showing line numbers:

    :set number
and 

    :set nonumber

to hide.

These commands can also be used as the shorthand `:set nu` and `:set nonu`.

If you wish to activate the feature which shows relative line numbers instead you can use

    :set relativenumber

or a shorthand `:set rnu`. Remember that you can mix `set relativenumber` with `set number`.


## Allocating conflicting keystrokes to IdeaVim
By default, some keystrokes that are useful in Vim contradict with the keystrokes of IntelliJ.

For example, `^R` in Vim is 'redo', but in IntelliJ it's the shortcut for `Run`

To decide which program interprets the keystroke, go to `Preferences -> Other Settings -> Vim Emulation` and choose which keystrokes to use with IdeaVim and which to use with IntelliJ:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/FTr5L.png

