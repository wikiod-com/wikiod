---
title: "Linux Named Pipes(FIFO)"
slug: "linux-named-pipesfifo"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## What is Named Pipe (FIFO)
    A named pipe is really just a special kind of file (a FIFO file) on the local hard drive. Unlike a regular file, a FIFO file does not contain any user information. Instead, it allows two or more processes to communicate with each other by reading/writing to/from this file.
    
    A named pipe works much like a regular pipe, but does have some noticeable differences.

    Named pipes exist as a device special file in the file system.
    Processes of different ancestry can share data through a named pipe.
    When all I/O is done by sharing processes, the named pipe remains in the file system for later use.

    The easiest way to create a FIFO file is to use the mkfifo command. This command is part of the standard Linux utilities and can simply be typed at the command prompt of your shell. You may also use the mknod command to accomplish the same thing.

