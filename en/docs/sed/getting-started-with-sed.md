---
title: "Getting started with sed"
slug: "getting-started-with-sed"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
One of the most common use of Sed is text substitution that can be achieved with the `s` command. 

In a terminal, type `echo "Hello sed" | sed 's/sed/World/'` and press <kbd>Enter</kbd>:

    $ echo "Hello sed" | sed 's/sed/World/'
    Hello World

"Hello World" should be output to the terminal.

The string "Hello, sed" is sent via pipe as input to the sed command that replace the word `sed` with `World`.

The syntax of a basic substitution command is `s` followed by the string or pattern to be searched and the substitution text. `s` command and strings are separated with a default `/` delimiter.


