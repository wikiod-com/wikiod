---
title: "Searching in the current buffer"
slug: "searching-in-the-current-buffer"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Searching for the word under the cursor
In normal mode, move the cursor to any word then press `*` to search forwards for the next occurrence of the word under the cursor, or press `#` to search backwards.

`*` or `#`  search for the exact word under the cursor: searching for `big` would only find `big` and not `bigger`.

Under the hood, Vim uses a simple search with *word boundaries* atoms:

- `/\<big\>` for `*`,
- `?\<big\>` for `#`.

`g*` or `g#` don't search for the exact word under the cursor: searching for `big` would find `bigger`.

Under the hood, Vim uses a simple search without *word boundaries* atoms:

- `/\<big\>` for `*`,
- `?\<big\>` for `#`.

## Searching for an arbitrary pattern
Vim's standard search commands are `/` for forward search and `?` for backward search.

To start a search from normal mode:

1. press `/`,
2. type your pattern,
3. press `<CR>` to perform the search.

Examples:

    /foobar<CR>      search forward for foobar
    ?foo\/bar<CR>    search backward for foo/bar

`n` and `N` can be used to jump to the next and previous occurence:

- Pressing `n` after a forward search positions the cursor on the next occurence, *forwards*.

- Pressing `N` after a forward search positions the cursor on the next occurence, *backwards*.

- Pressing `n` after a backward search positions the cursor on the next occurence, *backwards*.

- Pressing `N` after a backward search positions the cursor on the next occurence, *forwards*.

## execute command on lines that contain text
The `:global` command already has its own topic: https://www.wikiod.com/vim/global

