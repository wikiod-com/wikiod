---
title: "Input Output in Scheme"
slug: "input-output-in-scheme"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Input and Output in scheme is usually handled trough ports. A port is a data structure which is used to interact with the world outside Scheme. A Port isn't limited to files but can be used to read/write to sockets.

In some ways, the port object is some kind of universal object that can not only manipulate file and sockets but any kind of read/write operation with the OS.

For example, one could implement a port that can write to a printer or even control a CNC machine from Scheme using a port.

## Create an input port
An input port can be created in many ways, but usually the method starts with `open-input-`.

# String port

You can use a string as a port using `open-input-string`. It will create a port that will be able to read from the string.

    (define p
      (open-input-string "(a . (b . (c . ()))) 34"))

# File port

You can open a file for reading with `open-input-file`.

    (define p
      (open-input-file "path/to/file"))

## Read from an input port
Reading from an input port can be done in many ways. We can use the `read` method used by the REPL. It will read and interpret space separated expressions.

Taking the example from the string port above. We can read from the port like this:

    (define p
      (open-input-string "(a . (b . (c . ()))) 34"))
    (read p) -> (a b c)
    (read p) -> 34

We can read from a port as `char` using the special method `read-char`. This will return a single char from the port that we're reading from.

    (define p (open-input-string "hello"))
    (read-char p) -> #\h

