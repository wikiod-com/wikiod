---
title: "Help Within Emacs"
slug: "help-within-emacs"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Emacs is described as a self-documenting editor, and provides lots of information on how to use it within the editor itself. Amongst the entry points to this documentation is a tutorial, information about what functions is available related to a given topic,a information about the bindings between keystrokes and functions.The documentation is accessed using the prefix <kbd>C-h</kbd>, *i.e.* <kbd>Ctrl h</kbd>, or <kbd>F1</kbd>, with a list of further choices available by pressing <kbd>?</kbd>

## Available Functions and Key Bindings
Pressing <kbd>C-h a</kbd> will run the emacs function `apropos-command` which makes emacs prompt for words (or a regexp) to search for. It will then show a buffer containing a list of names and descriptions related to that topic, including key bindings for each of the functions available via keystrokes.

Pressing <kbd>C-h m</kbd> (`describe-mode`) gives a buffer describing the major and minor modes in effect, including listings of available functions and their key bindings.

Pressing <kbd>C-h b</kbd> (`describe-bindings`) gives a buffer listing all current key bindings. The listing includes global bindings as well as bindings for the active major and minor modes in the current buffer.

## Emacs Tutorial
<kbd>C-h t</kbd> runs the function `help-with-tutorial`, which opens a buffer containing a tutorial on the basic editing functionality of emacs, including moving around in text, and working with files, buffers, and windows.

## Key Binding Documentation
<kbd>C-h k</kbd> runs the function `describe-key`, which looks up the function mapped to the key strokes provided, and presents a description of the function which will be run when these keys are pressed.

<kbd>C-h c</kbd> runs the function `describe-key-briefly`, which only displays the function name mapped to given key sequence.

## Function Documentation
<kbd>C-h f</kbd> runs the function `describe-function`, which displays information on the usage and purpose of a given function. This is especially useful for functions that do not have a mapped key binding that can be used for documentation lookup via <kbd>C-h k</kbd>.

