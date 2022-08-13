---
title: "Converting text files from DOS to UNIX with vi"
slug: "converting-text-files-from-dos-to-unix-with-vi"
draft: false
images: []
weight: 9796
type: docs
toc: true
---

The `^M` character stands for a carriage return in Vim (`<c-m>` or just `<CR>`).  Vim displays this character when at least on line in the file uses `LF` line endings.  In other words, when Vim consider a file to have `fileformat=unix` but some lines do have carriage returns (`CR`), the carriage returns are displayed as `^M`.

A file that has a single line with `LF` line ending and several lines with `CRLF` line endings is most often created by wrongly editing a file created on a MSDOS based system.  For example, by creating a file under an MSDOS operating system, copying it to a UNIX based system, and then prepending a hash-bang sting (e.g. `#!/bin/sh`) using tools on the UNIX based operating system.

## Converting a DOS Text file to a UNIX Text file
Quite often you have a file which was edited within DOS or Windows and you are viewing it under UNIX.  This can look like the following when you view the file with vi.

    First line of file^M
    Next Line^M
    And another^M

If you wish to remove the ^M, it can be that you delete each ^M by hand.  Alternatively, in vi after hitting <kbd>Esc</kbd> you can enter the following at the command mode prompt:

    :1,$s/^M//g

Where ^M is entered with <kbd>Ctrl</kbd> and <kbd>v</kbd> together and then <kbd>Ctrl</kbd> and <kbd>m</kbd> together.

This executes the command from the first line '1' to the last line '$', the command is to substitute 's' the '^M' for nothing '' and to this globally 'g'.

## Using VIm's fileformat
When Vim opens a file with `<CR><NL>` line endings (common on MSDOS based operating systems, also called `CRLF`) it will set `fileformat` to `dos`, you can check what with:

    :set fileformat?
      fileformat=dos

Or just

    :set ff?
      fileformat=dos

To convert it to `<NL>` line endings (common on most UNIX based operating systems, also called `LF`) you can change the `fileformat` setting and Vim will change the buffer.

    :set ff=unix

