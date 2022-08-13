---
title: "Filetype plugins"
slug: "filetype-plugins"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Where to put custom filetype plugins?
Filetype plugins for `foo` filetype are sourced in that order:

1. `$HOME/.vim/ftplugin/foo.vim`. Be careful with what you put in that file as it may be overridden by `$VIMRUNTIME/ftplugin/foo.vim` -- under windows, it'll be instead `$HOME/vimfiles/ftplugin/foo.vim`

2. `$VIMRUNTIME/ftplugin/foo.vim`. Like everything under `$VIMRUNTIME`, this file should not be changed.

3. `$HOME/.vim/after/ftplugin/foo.vim`. This file comes last so it's the ideal place for *your* filetype-specific settings.

