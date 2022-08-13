---
title: "Getting started with zsh"
slug: "getting-started-with-zsh"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
# Getting `zsh`

`zsh` is available on many UNIX-like platforms via their built-in package management systems. On the Debian and Ubuntu Linux distributions, `zsh` is available in the default package repositories and can be installed using:

    $ sudo apt-get install zsh
    # or, on newer Ubuntu distributions
    $ sudo apt install zsh

On RPM-based distributions, `zsh` is also often available in the default package archives and can be installed using:

    $ yum install zsh

On Fedora 22 and later:

    $ dnf install zsh

On BSD systems, `zsh` can be installed using `pkg`:

    $ pkg install zsh

On OpenBSD, `zsh` can be installed using `pkg_add`:

    $ pkg_add zsh

On Arch Linux, `zsh` can be installed using `pacman`:

    $ pacman -S zsh

On openSUSE, `zsh` can be installed using `zypper`:

    $ zypper install zsh

On systems running macOS (OS X) `zsh` is already installed by default, although not set as default shell. You can also install newer versions via Homebrew:

    $ brew install zsh

Alternatively, `zsh`'s source code can be obtained from the [official website](http://zsh.sourceforge.net/).

From there, the shell can be started by typing `zsh` at the prompt.

# Making `zsh` your default shell

On most Linux and BSD systems, `zsh` may be set as the default shell for a user using the `chsh` command:

    $ chsh -s shell [username]

Where 
* `username` is a real username (defaults to the current user if left out) 
* `shell` is the path to the `zsh` binary. The path should be listed in the `/etc/shells` file, which contains a list of allowed shells for use with `chsh`. Should `zsh` not be listed there - for example because you compiled and installed it from source - you will need to add a line with the absolute path to `zsh` first. You can get this path with `which zsh` (provided it is installed in a directory listed in `PATH`)

In order to see the changes log out once and log in. Open the terminal emulator and use

    `echo $SHELL`

If it displays `/bin/zsh` then you have successfully changed the default shell to zsh.



## Configuration
When starting Zsh, it'll source the following files in this order by default:

1. `/etc/zsh/zshenv` Used for setting system-wide environment variables; it should not contain commands that produce output or assume the shell is attached to a tty. This file will always be sourced, this cannot be overridden.

2. `$ZDOTDIR/.zshenv` Used for setting user's environment variables; it should not contain commands that produce output or assume the shell is attached to a tty. This file will always be sourced.

3. `/etc/zsh/zprofile` Used for executing commands at start, will be sourced when starting as a login shell.
> Note that on Arch Linux, by default it contains one line which source the /etc/profile.

> `/etc/profile` This file should be sourced by all Bourne-compatible shells upon login: it sets up `$PATH` and other environment variables and application-specific (/etc/profile.d/*.sh) settings upon login.

4. `$ZDOTDIR/.zprofile` Used for executing user's commands at start, will be sourced when starting as a login shell.

5. `/etc/zsh/zshrc` Used for setting interactive shell configuration and executing commands, will be sourced when starting as a interactive shell.

6. `$ZDOTDIR/.zshrc` Used for setting user's interactive shell configuration and executing commands, will be sourced when starting as a interactive shell.

7. `/etc/zsh/zlogin` Used for executing commands at ending of initial progress, will be sourced when starting as a login shell.

8. `$ZDOTDIR/.zlogin` Used for executing user's commands at ending of initial progress, will be sourced when starting as a login shell.

9. `$ZDOTDIR/.zlogout` Will be sourced when a login shell exits.

10. `/etc/zsh/zlogout` Will be sourced when a login shell exits.

> If $ZDOTDIR is not set, $HOME is used instead.

> For general personal use, it is typical to edit the user's `.zshrc` file for personal preferences

## Aliases 
To alias a command in you `~/.zshrc` file, you can use the following syntax:

    alias [alias-name]="[command-to-execute]"

For example, it is common to execute the command `ls -a`. You can alias this command as `la` as such:

    alias la="ls -a"

After reloading the `~/.zshrc` file, you will be able to type `la` and `ls -a` will be executed.

# Directory Aliases
It is common to have certain folders that you `cd` into often. If this is the case, you can create aliasses to those directories to make `cd`ing to them easier. For example, the following will alias the Dropbox folder:

    alias db="cd ~/Dropbox"

allowing you to enter `db` and change directories to `~/Dropbox`.

## Reload ZSH Configuration
zsh loads configuration from the `~/.zshrc` file on startup. If you make changes to that file, you can either restart zsh or run the following command to reload the configuration.

    . ~/.zshrc

You can alias this useful command in your `~/.zshrc` like this:

    alias reload=". ~/.zshrc"

