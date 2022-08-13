---
title: "Getting started with vim"
slug: "getting-started-with-vim"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Exiting Vim
In order to exit Vim, first make sure you are in *Normal* mode by pressing <kbd>Esc</kbd>.

 - `:q` <kbd>Enter</kbd> (will prevent you from exiting if you have unsaved changes - short for :quit)

To *discard* changes and exit Vim:

 - `:q!` <kbd>Enter</kbd> to force exit and discard changes (short for `:quit!`, not to be confused with `:!q`),
 - `ZQ` is a shortcut that does the same as `:q!`,
 - `:cq` <kbd>Enter</kbd> quit and return error (discard all changes so the compiler will not recompile this file)

To *save* changes and exit Vim:

 - `:wq` <kbd>Enter</kbd> (shorthand for `:write` and `:quit`),
 - `:x` <kbd>Enter</kbd> (same as `:wq`, but will not write if the file was not changed),
 - `ZZ` is a shortcut that does the same as `:x` (Save workspace and quit the editor),
 - `:[range]wq!` <kbd>Enter</kbd> (write the lines in [range])

To close multiple buffers at once (even in multiple windows and/or tabs), append the letter `a` to any of the *Commands* above (the ones starting with `:`). For example, to write and quit all windows you can use:

  - `:wqa` <kbd>Enter</kbd> or
  - `:xa` <kbd>Enter</kbd> &mdash; Write all changed buffers and exit Vim.  If there are buffers without a file name, which are readonly or which cannot be written for another reason, Vim will not quit
  - `:xa!` <kbd>Enter</kbd> &mdash; Write all changed buffers, even the ones that are readonly, and exit Vim.  If there are buffers without a file name or which cannot be written for another reason, Vim will not quit
  - `:qa` <kbd>Enter</kbd> &mdash; try to quit, but stop if there are any unsaved files;
  - `:qa!` <kbd>Enter</kbd> &mdash; quit *without saving* (discard changes in *any* unsaved files)

If you have opened Vim without specifying a file and you want to save that file before exiting, you will receive `E32: No file name` message. You can save your file and quit using:

 - `:wq filename` <kbd>Enter</kbd> or;
 - `:x filename` <kbd>Enter</kbd>

## Explanation:

The <kbd>:</kbd> keystroke actually opens *Command* mode. The command `q` is an abbreviation of `quit`, `w`, of `write` and `x`, of `exit` (you can also type `:quit`, `:write` and `:exit` if you want). Shortcuts _not_ starting with `:` such as `ZZ` and `ZQ` refer to *Normal* mode key mappings. You can think of them as shortcuts.

The `!` keystroke is sometimes used at the end of a command to force its execution, which allows to discard changes in the case of `:q!`.
Placing the `!` at the beginning of the command has a different meaning.
For example, one can mistype `:!q` instead of `:q!` and vim would terminate with a 127 error.

An easy way to remember this is to think of `!` as a way of insisting on executing something.
Just like when you write: "I want to quit!"

## Basics
Run interactive [vim tutorials][1] as many times as needed to feel comfortable with the basics.

Vim features several modes, e.g. **normal mode**, **insert mode** and **command-line mode**.

**Normal mode** is for editing and navigating text. In this mode `h`, `j`, `k` and `l` correspond to the cursor keys `←`, `↓`, `↑` and `→`. Most commands in normal mode can be prefixed with a "count", e.g. `3j` moves down 3 lines.

**Insert mode** is for inserting the text directly, in this mode vim is similar to other more simple text editors. To enter insert mode press `i` in normal mode. To leave it press `<ESC>` (escape key).

**Command-line mode** is for running more complex commands like saving the file and exiting vim. Press `:` to start the command-line mode. To leave this mode you can also press `<ESC>`. To save the changes to the file use `:w` (or `:write`). To exit vim without saving your changes use `:q!` (or `:quit!`).

These are some of the more useful commands in vim: 

Command | Description
---|---
`i` | (insert) enters insert mode *before* the current cursor position
`I` | enters insert mode *before* the first printable character of the current line
`a` | (append) enters insert mode *after* the current cursor position
`A` | enters insert mode *after* the last printable character of the current line
`x` | delete character at the current cursor position
`X` | delete character at the left to the current cursor position
`w` | move to next word 
`b` | move to previous word
`0` | move to the beginning of line 
`$` | move to the end of line
`r` | replace – enters replace mode for one character. The next character you type will replace the character under the cursor.
`R` | enters replace mode indefinitely. Every character you type will replace the character under the cursor and advance the cursor by one.
`s` | substitute – deletes the character at the current cursor position and then enters insert mode
`S` | delete the current line that the cursor is currently on and enter insert mode
`<Esc>`, `<C-c>` | exit insert mode and returns to normal mode
`u` | undo
`<C-r>` | redo
`dd`, `dw`, `dl`, `d$` | cut the current line, from the cursor to next word, or the character, current position to end of current line respectively, note: `D` is the equivalent of `d$`
`cc`, `cw`, `cl` | change the current line, from the cursor to next word, or the character, respectively
`yy`, `yw`, `yl`, `y$` | yank ("copy") the current line, from the cursor to next word, or the character, current position to end of current line respectively
`p`, `P` | put ("paste") after, or before current position, respectively
`o`, `O` | to create a new empty line, after or before the current one and enter insert mode
`:w` | write the current buffer to disk
`:q!`, `ZQ` | quit without writing
`:x`, `:wq`, `ZZ` | write and quit
`:help` | open a window with help file
`:help {subject}` | show help for a specific subject
`qz` | begin recording actions to register `z`, `q` to end recording, `@z` to play back the actions. `z` can be any letter: `q` is often used for convenience. Read more: [Macros](https://www.wikiod.com/vim/macros)


  [1]: https://www.wikiod.com/vim/getting-started-with-vim#Interactive Vim Tutorials (such as vimtutor)

## Saving a read-only file edited in Vim
Sometimes, we may open a file which we do not have permission to write in Vim without using `sudo`.

Use this command to save a read-only file edited in Vim.

    :w !sudo tee > /dev/null %

Which you could map to `:w!!` in your `.vimrc`:

    cmap w!! w !sudo tee > /dev/null %

You will be presented a prompt as shown in the image.

 [![Press ENTER or type command to continue. [O]K, (L)oad File:][1]][1]. 

Press `O` and the file will be saved. It remains open in vi/vim for more editing or reading and you can exit normally by typing `:q!` since the file is still open as read-only.

## Command Explanation 

    :w ............................ isn't modifying your file in this case, 
       ............................ but sends the current buffer contents to 
       ............................ a substituted shell command
       !sudo ...................... call the shell 'sudo' command
             tee .................. the output of the vi/vim write command is redirected 
                                    using the 'tee' command
                 > /dev/null ...... throws away the standard output, since we don't need 
                                    to pass it to other commands
                             % .... expands to the path of the current file

Sources:

- [Adam Culp's Tech Blog](http://www.geekyboy.com/archives/629)
- [Stackoverflow, How does the vim "write with sudo" trick work](http://stackoverflow.com/a/7078429/5773806)

  [1]: http://i.stack.imgur.com/AUQss.png

## Interactive Vim Tutorials (such as vimtutor)
`vimtutor` is an interactive tutorial covering the most basic aspects of text editing.

On UNIX-like system, you can start the tutorial with:

    $ vimtutor

On Windows, “Vim tutor” can be found in the “Vim 7.x” directory under “All Programs” in the Windows menu.

See `:help vimtutor` for further details.

Other interactive tutorials include these browser-based ones:

- [Vim Adventures](https://vim-adventures.com/) – An interactive game version of vimtutor available on the web. Only the first few levels are free.
- [Open Vim](http://www.openvim.com/) – An interactive terminal which teaches you the basic commands with feedback.
- [Vim Genius](http://www.vimgenius.com/) – Another interactive terminal which also includes intermediate and advanced lessons including macros and arglist.


## Installation
The Vim on your machine—if there is one—is very likely to be a "small" build that lacks useful features like clipboard support, syntax highlighting or even the ability to use plugins.

This is not a problem if all you need is a quick way to edit config files but you will soon hit a number of walls if you intend to make Vim your main editor.

It is therefore generally recommended to install a complete build.

## Installation on Linux/BSD

On those systems, the trick is simply to install the GUI version which comes with both a `gvim` command for starting the GUI and a `vim` command for starting the TUI.

### Arch and Arch-based distributions

    $ sudo pacman -R vim
    $ sudo pacman -S gvim

### Debian and Debian-based distributions

    $ sudo apt-get update
    $ sudo apt-get install vim-gtk

### Gentoo and Gentoo-based distributions

    $ sudo emerge --sync
    $ sudo emerge app-editors/gvim

### RedHat and RedHat-based distributions

    $ sudo yum check-update
    $ sudo yum install vim-X11

### Fedora

    $ sudo dnf check-update
    $ sudo dnf install vim-X11

### Slackware and Slackware-based distributions

    $ sudo slackpkg update
    $ sudo slackpkg install-new vim-gvim

### OpenBSD and OpenBSD-based distributions

    $ sudo pkg_add vim-x11

### FreeBSD and FreeBSD-based distributions

    $ sudo pkg install editors/vim

## Installation on Mac OS X

The strategy is similar to Mac OS X: we install the GUI version to get both the GUI and the TUI. In the end, we should be able to:

* double-click the MacVim icon in the Finder,
* click on the MacVim icon in the Dock,
* issue `$ mvim` in the shell to open the MacVim GUI,
* issue `$ mvim -v` in the shell to open the MacVim TUI.

### Regular install

Download and install [an official snapshot][1] like you would with any other Mac OS X application.

Place the `mvim` script that comes bundled with MacVim somewhere in your `$PATH`.

### Package manager

#### MacPorts:

    $ sudo port selfupdate
    $ sudo port install macvim

#### Homebrew:

    $ brew install macvim

To make MacVim the default console Vim:

    $ brew install macvim --with-override-system-vim

## Installation on Windows

There is no Vim on Windows systems by default. You can download and install Vim from the [Tuxproject site][1] for more up-to-date and complete builds or you can download and install Vim from the official [Vim site][2].

### Chocolatey

    > choco install vim

## Building Vim from source

If the methods above don't suit your needs it is still possible to build Vim yourself, with *only* the options you need.

This topic will be discussed in its own section (currently in draft).


  [1]: https://github.com/macvim-dev/macvim/releases
  [2]: http://www.vim.org/download.php#pc
  [3]: https://www.wikiod.com/vim

## Suspending vim
When using `vim` from the command line, you can suspend `vim` and get back to your prompt, without actually quitting `vim`. Hence you will later be able to get back your `vim` session from the same prompt.

When in *Normal mode* (if not, press <kbd>esc</kbd> to get there), issue either of these commands:

> `:st`<kbd>enter</kbd>
>
> `:sus`<kbd>enter</kbd>
>
> `:stop`<kbd>enter</kbd>
>
> `:suspend`<kbd>enter</kbd>


Alternatively, on some systems, when in *Normal* or *Visual* mode, issuing <kbd>Ctrl</kbd>+<kbd>Z</kbd> will have the same effect.

**Note:** If `autowrite` is set, buffers with changes and filenames will be written out. Add a `!` before <kbd>enter</kbd> to avoid, eg. `:st!`<kbd>enter</kbd>.

Later, when you want to return to your `vim` session, if you haven't suspended any other jobs, issuing the following will restore vim as your foreground job.

> `fg`<kbd>enter</kbd>

Otherwise you will need to find your `vim` sessions's job ID by issuing `jobs`<kbd>enter</kbd> and then foregrounding the matching jobs `fg %[job ID]`<kbd>enter</kbd> eg. `fg %1`<kbd>enter</kbd>.

## What to do in case of a crash
Vim saves all your unsaved edits in a *swap file*, an extra file that gets deleted once the changes are committed by saving. The name of the swap file is usually the name of the file being edited preceded by a `.` and with a `.swp` suffix (you can see it with `:sw`).

So in case your vim process terminates before you've had the chance to save your edits you can recover your work by applying the changes contained in the swap file to your current file by using the command-line option `-r`. For instance if `myFile` is the file you were editing, use:

    $ vi -r myFile

to recover the uncommitted changes. 

If a swap file exists, vim should prompt you anyway for recovery options
    
    $ vi myFile
    E325: ATTENTION
    Found a swap file by the name ".myFile.swp"
    ...
    Swap file ".myFile.swp" already exists!
    [O]pen Read-Only, (E)dit anyway, (R)ecover, (D)elete it, (Q)uit, (A)bort:

If you choose (R)ecover then the changes from the `swp` file are applied but the swap file won't be deleted, so don't forget to delete the swap file afterwards if you're satisfied with the recovery.

