---
title: "Modes - insert, normal, visual, ex"
slug: "modes---insert-normal-visual-ex"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## The basics about modes
`vim` is a modal editor. This means that at any time inside a `vim` session, the user is going to be in one of the modes of operation. Each one of offers a different set commands, operations, key bindings...

## Normal mode (or Command mode)

- The mode `vim` starts in.
- From other modes, usually accessible by `Esc`.
- **Has most of the navigation and text manipulation commands.**

See `:help normal-mode`.

## Insert mode

- Commonly accessed by: `a`, `i`, `A`, `I`, `c`, `s`.
- **For inserting text**.

See `:help insert-mode`.

## Visual mode

- Commonly accessed by: `v` (characterwise), `V` (linewise), `<C-v>` (blockwise).
- Basically, for **text selection**; most normal commands are available, plus extra ones to act on the selected text.

See `:help visual-mode`.

## Select mode

- Accessible from insert mode with `<C-g>`.
- Similar to visual mode but with a lot less available commands.
- Contrary to insert mode, it is possible to type right away.
- Rarely used.

See `:help select-mode`.

## Replace mode

- Accessible from normal mode with `R`.
- Allows to overwrite existing text.

See `:help replace-mode`.

## Command-line mode

See `:help command-line-mode`.

## Ex mode

See `:help Ex-mode`.


