---
title: "Type of Shells"
slug: "type-of-shells"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

**Login Shell**

A login shell is one whose first character of argument zero is a -, or one started with the –login option. 
The Initialization is more comprehensive than in an normal interactive (sub) shell.

**Interactive Shell**

An interactive shell is one started without non-option arguments and without the -c option whose standard input and error are both connected to terminals (as determined by isatty(3)), or one started with the -i option. PS1 is set and $- includes i if bash is interactive, allowing a shell script or a startup file to test this state.

**non-interactive Shell**

A non-interactive Shell is a shell in which the user can not interact with the shell. As en example, a shell running a script is always a non-interactive shell. All the same, the script can still access its tty.

**Configuring a login shell**

On logging in: 

    If '/etc/profile' exists, then source it. 
    If '~/.bash_profile' exists, then source it, 
    else if '~/.bash_login' exists, then source it, 
    else if '~/.profile' exists, then source it. 

**For non-login interactive shells**

On starting up: 

    If `~/.bashrc' exists, then source it.


**For non-interactive shells**

On starting up: 
If the environment variable ENV is non-null, expand the variable and source the file named by the value. If Bash is not started in Posix mode, it looks for BASH_ENV before ENV.

## Introduction to dot files


## Start an interactive shell
    bash

## Detect type of shell
    shopt -q login_shell && echo 'login' || echo 'not-login'


