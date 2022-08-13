---
title: "Batch file macros"
slug: "batch-file-macros"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

In a command prompt, you can use DOSKEY for creating macros. In a batch file you can define a variable that can be called as a piece of code and even pass arguments to it.

## Basic Macro
Using `DOSKEY`, we can create macros to simplify typing many commands in command prompt. Take a look at the following example.

    DOSKEY macro=echo Hello World

Now if you type `macro` in the command prompt, it would return `Hello World`.

## Comments
Unfortunately, `DOSKEY` macro doesn't support comment, but there's a workaround.

    ;= Comment
    ;= Comment
    ;= Remember to end your comment with ;=
    ;=

## $ Character Usages
There are 3 usages of the `$` character in a `DOSKEY` macro.

---

# Command separator

`$T` is the equivalent of `&` in a batch script. One can join commands together like so.

    DOSKEY test=echo hello $T echo world

---

# Command-line arguments

Like `bash`(not `batch`), we use `$` to indicate command-line argument.

`$1` refers to the first command-line argument

`$2` refers to    second command-line argument, etc..

`$*` refers to all command-line argument



## Macros In Batch Script
`DOSKEY` macros don't work in a batch script. However, we can use a little workaround.

    set DOSKEYMacro=echo Hello World
    %DOSKEYMacro%

This script can simulate the macro function. One can also use ampersands(`&`) to join commands, like `$T` in `DOSKEY`.

If you want a relatively large "macro", you may try a [simple function][1] or take a look at other function topics [here][2].


  [1]: https://www.wikiod.com/batch-file/functions#Simple Function
  [2]: https://www.wikiod.com/batch-file/functions

