---
title: "Motions and Text Objects"
slug: "motions-and-text-objects"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

A text object in Vim is another way to specify a chunk of text to operate on. They can be used with operators or in visual mode, instead of motions.

## Changing the contents of a string or parameter list
Let's say you have this line of code:

    printf("Hello, world!\n");

Now say you want to change the text to "Program exiting."

Command | Buffer | Mnemonic
--------|--------|-----------------------
`ci"` | `printf("¦");` | **c**hange **i**n the **"**.
`Program exiting.\n<esc>` | `printf("Program exiting.\n");`

