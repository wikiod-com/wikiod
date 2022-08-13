---
title: "Emacs nomenclature"
slug: "emacs-nomenclature"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Elements of the User Interface
Emacs's user interface uses terms that were coined early and can be unsettling to users used to a more modern terminology.

[![Emacs terminology][1]][1]

# Frame

In Emacs, what is otherwise called a window (the area of the display used by a program) is called a *frame*. Emacs starts using one *frame*, though additional frames may be created using <kbd>C-x 5</kbd>.


# Window

A *frame* contains one or more *windows* (otherwise usually called panes), each showing the content of one *buffer*. Each *frame* usually starts with only one *window*, but additional windows can be created by splitting existing ones ; either horizontally using <kbd>C-x 2</kbd> or vertically with <kbd>C-x 3</kbd>. See also https://www.wikiod.com/emacs/basic-keybindings#Multiples windows or frames


# Buffer

The term *buffer* refers to the content displayed in a *window*. Such content may reflect the content of a file in the file system (or maybe an updated version that has not been saved to the disk yet), but more generally it can be any kind of text.


# Mode line

At the bottom of each *window* is a *mode line*, which synthetically describes the *buffer* displayed in the window.

# Tool Bar

In a similar way to many other softwares, a *tool bar* can be displayed at the top of each *frame*. Its contents may vary depending on the type of *buffer* being currently edited.

# Minibuffer

A *minibuffer*, usually displayed at the bottom of each *frame*, allows interacting with Emacs. Each time a command asks for user input, it is prompted in the *minibuffer*. Conversely, messages displayed for the user to see are printed there.

  [1]: http://i.stack.imgur.com/hIeNf.png

## Point, mark and region
Emacs uses the terms **point**, **mark**, and **region** to provide more precision about the selected text and position of the cursor. By understanding these terms, it'll help you understand and use other operations and functions.

The **point** is the place in a buffer where editing (_i.e._ insertion) is currently taking place, and is usually indicated by a cursor.  
The **mark** is a marker placed anywhere in the buffer using commands like `set-mark-command` (<kbd>C-SPC</kbd>) or `exchange-point-and-mark` (<kbd>C-x C-x</kbd>).  
The **region** is the area between point and mark, and many commands operate on the region to _e.g._ delete, spell check, indent, or compile it.

When you click your mouse on a location in a buffer, you're seeing the point. When you select text, you're setting the region (the selected text) and the mark (at the beginning of your selection).

## Files and buffers
In Emacs, *file* has the same meaning as in the operating system, and is used for permanent storage of data. A *buffer* is the internal representation of a file being edited. Files can be read into buffers using <kbd>C-x C-f</kbd>, and buffers can be written to files using <kbd>C-x C-s</kbd> (save file at its current location) or <kbd>C-x C-w</kbd> (write file to a different location, prompting for it - the equivalent of <kbd>Save as</kbd>).

## Killing and yanking
*Killing* and *yanking* more or less correspond to what is usually called "cutting" and "pasting".

# Killing 

*killing* means deleting text, and copying it to the *kill-ring* (which could be seen as a sort of "clipboard" in the "cut & paste" terminology). The *kill ring* is so named because it stores several pieces of *killed* text, which can later be accessed in cyclic order.

Various commands exist that *kill* one word (<kbd>M-d</kbd>), the rest of the line (<kbd>C-k</kbd>), or larger text blocks (such as the currently selected region: <kbd>C-w</kbd>).

Other commands exist, that save text to the *kill ring*, without actually *killing* it (in a similar way to "copying" in modern terminologies). For example, <kbd>M-w</kbd>, which acts on the currently selected region.

# Yanking

Entries in the *kill ring* can later be *yanked* back into a buffer. One can typically *yank* the most recently *killed* text using *e.g* <kbd>C-y</kbd> (which is similar to the "paste" operation in a more modern terminology). But other commands can access and *yank* older entries from the *kill ring*.

## Modes
# Major mode

Emacs can adapt its behaviour to the specific type of text edited in a *buffer*. The set of specific Emacs customizations for a particular type of text is called a "major mode". Each buffer has exactly one *major mode* depending on its content type.

*Major modes* can change the meaning of some keys, define syntax highlighing or indentation rules, and install new key bindings (usually beginning with <kbd>C-c</kbd>) for mode-specific commands. Emacs ships with a wide range of major modes, falling into three main categories:

   - support for text (e.g. markup languages),
   - support for programming languages,
   - applications within emacs (e.g. dired, gnus, ...). Buffers using this last group of major modes are usually not associated to files, but rather serve as a user interface.


# Minor mode

*Minor modes* are optional features that can be turned on and off. *Minor modes* can be enabled for specific buffers (buffer-local modes) or all buffers (global modes). In contrast to *major modes* any number of *minor mode* can be activated for a given buffer.

Emacs provides lots of minor modes. A few examples include:

   - *Auto-fill mode* to automatically wrap text lines as you type.
   - *Flyspell mode* to highlight spelling errors as you type.
   - *Visual Line mode* to wrap long lines to fit the screen.
   - *Transient Mark mode* to highlight the current region.


