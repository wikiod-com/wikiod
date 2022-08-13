---
title: "Change shell"
slug: "change-shell"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- echo $0
- ps -p $$
- echo $SHELL
- export SHELL=/bin/bash
- exec /bin/bash
- cat /etc/shells

## Find the current shell
There are a few ways to determine the current shell

    echo $0
    ps -p $$
    echo $SHELL

## Change the shell
To change the current bash run these commands

    export SHELL=/bin/bash
    exec /bin/bash

to change the bash that opens on startup edit `.profile` and add those lines

## List available shells
To list available login shells :

    cat /etc/shells

Example:

    $ cat /etc/shells
    # /etc/shells: valid login shells
    /bin/sh
    /bin/dash
    /bin/bash
    /bin/rbash



