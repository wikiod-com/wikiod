---
title: "Using Python for Vim scripting"
slug: "using-python-for-vim-scripting"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
  - :[range]py[thon] {statement}

## Check Python version in Vim
Vim has its own built-in Python interpreter. Thus it could use a different version of the default interpreter for the operating system.

To check with which version of Python Vim was compiled, type the following command:

    :python import sys; print(sys.version)

This imports the `sys` module and prints its `version` property, containing the version of the currently used Python interpreter.

## Executing multi-line Python code
Every Python statement in Vim should be prefixed with the `:python` command, to instruct Vim that the next command is not Vimscript but Python.

To avoid typing this command on each line, when executing multi-line Python code, it is possible to instruct Vim to interpret the code between two marker expressions as Python.

To achieve this, use:

    :python << {marker_name}
    a = "Hello World"
    print(a)
    {marker_name}

where `{marker_name}` is the word you want to use to designate the end of the python block.

E.g.:

    :python << endpython
    surname = "Doe"
    forename = "Jane"
    print("Hello, %s %s" % (forename, surname))
    endpython

would print:

    Hello, Jane Doe

## Execute Vim normal mode commands through Python statement
To be able to use vim commands in Python, the `vim` module should be imported.

    :python import vim
After having this module imported, the user has access to the `command` function:

    :python vim.command("normal iText to insert")

This command would execute `i` in normal mode then type `Text to insert` and fall back to normal mode.

