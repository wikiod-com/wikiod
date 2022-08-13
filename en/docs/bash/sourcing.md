---
title: "Sourcing"
slug: "sourcing"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sourcing a file
Sourcing a file is different from execution, in that all commands are evaluated within the context of the current bash session - this means that any variables, function, or aliases defined will persist throughout your session.

Create the file you wish to source `sourceme.sh`

    #!/bin/bash

    export A="hello_world"
    alias sayHi="echo Hi"
    sayHello() {
        echo Hello
    }

From your session, source the file

    $ source sourceme.sh

From hencefourth, you have all the resources of the sourced file available

    $ echo $A
    hello_world

    $ sayHi
    Hi

    $ sayHello
    Hello

Note that the command `.` is synonymous to `source`, such that you can simply use

    $ . sourceme.sh

## Sourcing a virtual environment
When developing several applications on one machine, it becomes useful to separate out dependencies into virtual environments.

With the use of [`virtualenv`][1], these environments are sourced into your shell so that when you run a command, it comes from that virtual environment.

This is most commonly installed using `pip`.

    pip install https://github.com/pypa/virtualenv/tarball/15.0.2

Create a new environment

    virtualenv --python=python3.5 my_env

Activate the environment

    source my_env/bin/activate


  [1]: https://github.com/pypa/virtualenv/blob/master/README.rst

