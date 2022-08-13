---
title: "Exiting Vim"
slug: "exiting-vim"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
|`:`|Enter command-line mode|
|`w`|Write|
|`q`|Quit|
|`a`|All|
|`!`|Override|


Command-line mode is entered through normal mode. You will know you are in command-line mode when there is a `:` in the bottom left corner of your terminal window.

Normal mode is the default mode of vi/vim and can be switched to by pressing the <kbd>ESC</kbd>.

Vi/Vim have built-in checks to prevent unsaved work from being lost and other useful features. To bypass these checks, use the override `!` in your command.

In Vi/Vim it is possible to have more than one file displayed (in different windows) at the same time. Use `a` to close all the opened windows.

## Exit forcefully (without save)
<kbd>:q!</kbd>  

<kbd>ZQ</kbd>

## Exit forcefully (with save)
<kbd>:wq!</kbd>

## Exit forcefully from all opened windows (without save)
<kbd>:qa!</kbd>

## Exit with save
<kbd>:wq</kbd>

<kbd>ZZ</kbd>

## Exit without save
<kbd>:q!</kbd>

## if multiple files are opened
    :wqall

Exiting multiple files with saving contents

    :qall!

Exiting multiple files without saving contents

