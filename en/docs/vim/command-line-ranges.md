---
title: "Command-line ranges"
slug: "command-line-ranges"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Absolute line numbers
The following command executes `:command` on lines `23` to `56`:

    :23,56command

NB: Ranges are *inclusive* by default.

## Relative line numbers
In the following command the range starts 6 lines above the current line and ends 3 lines below:

    :-6,+3command

## Line shortcuts
- `.` represents *the current line* but it can also be omitted entirely.
- `$` represents *the last line*.
- `%` represents *the whole buffer*, it is a shortcut for `1,$`.

The two commands below execute `:command` on every file from the current line to the last line:

    :.,$command
    :,$command

The command below executes `:command` on the whole buffer:

    :%command


## Marks
The command below executes `:command` on every line from the one containing the `f` manual mark to the one containing the `t` manual mark:

    :'f,'tcommand

Automatic marks can be used too:

    :'<,'>command    " covers the visual selection
    :'{,'}command    " covers the current paragraph
    :'[,']command    " covers the last changed text

See `:help mark-motions`.

## Search
The commands below execute `:command` on every line from the first matching `from` to the first matching `to`:

    :/from/,/to/command    " from next 'from' to next 'to'
    :?from?,/to/command    " from previous 'from' to next 'to'
    :?from?,?to?command    " from previous 'from' to previous 'to'

See `:help search-commands`.

## Line offsets
Line offsets can be used to adjust the start and end lines:

    :/foo/-,/bar/+4command    " from the line above next 'foo' to 4 lines below next 'bar'

See `:help search-offset`.

## Mixed ranges
It's possible to combine all of the above into expressive ranges:

    :1267,/foo/-2command
    :'{,command
    :'f,$command

Be creative and don't forget to read `:help cmdline-ranges`.

