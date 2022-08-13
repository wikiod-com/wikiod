---
title: "Getting started with sh"
slug: "getting-started-with-sh"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, world!
With `echo`:

    $ echo Hello, world!
    Hello, world!

With `printf`:

    $ printf 'Hello, world!\n'
    Hello, world!

As a file:

    #!/bin/sh
    printf '%s\n' 'Hello, world!'


## Echo Portability
    $ for shell in ash bash dash ksh ksh93 zsh; do
    >     $shell -c "echo '\\\\'$shell'\\\\'"
    > done
    \\ash\\
    \\bash\\
    \dash\
    \pdksh\
    \\ksh93\\
    \zsh\

'echo' can only be used consistently, across implementations, if its arguments do not contain any backslashes     (reverse-solidi), and if the first argument does not start with a dash (hyphen-minus).  Many implementations allow additional options, such as `-e`, even though the only option allowed is `-n` (see below).

From [POSIX][posix-echo]:

> If the first operand is -n, or if any of the operands contain a <backslash> character, the results are implementation-defined. 

[posix-echo]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/echo.html

