---
title: "Shell Scripting and Piping"
slug: "shell-scripting-and-piping"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntax
 - ;shell command

## Using Shell from inside the REPL
From inside the interative Julia shell (also known as REPL), you can access the system's shell by typing `;` right after the prompt:

```julia
shell>
```

From here on, you can type any shell comand and they will be run from inside the REPL:

```julia
shell> ls
Desktop     Documents   Pictures   Templates
Downloads   Music       Public     Videos
```

To exit this mode, type `backspace` when the prompt is empty. 

## Shelling out from Julia code
Julia code can create, manipulate, and execute command literals, which execute in the OS's system environment. This is powerful but often makes programs less portable.

A command literal can be created using the <code>``</code> literal. Information can be interpolated using the `$` interpolation syntax, as with string literals. Julia variables passed through command literals need not be escaped first; they are not actually passed to the shell, but rather directly to the kernel. However, Julia displays these objects so that they appear properly escaped.

    julia> msg = "a commit message"
    "a commit message"

    julia> command = `git commit -am $msg`
    `git commit -am 'a commit message'`

    julia> cd("/directory/where/there/are/unstaged/changes")

    julia> run(command)
    [master (root-commit) 0945387] add a
     4 files changed, 1 insertion(+)


