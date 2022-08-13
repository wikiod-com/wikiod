---
title: "Rebar3"
slug: "rebar3"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Installing Rebar3
Rebar3 is written in Erlang, so you need Erlang to run it. It is available as a binary that you can download and run. Just fetch the nightly build and give it execution permissions:

    $ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

Place this binary in a convenient place and add it to your path. For example, in a bin directory in your home:

    $ mkdir ~/bin && mv rebar3 ~/bin
    $ export PATH=~/bin:$PATH

This last line should be put in your `.bashrc`. As an alternative, one can also link the binary to `/usr/local/bin` directory, making it available as a normal command.

    $ sudo ln -s /path/to/your/rebar3 /usr/local/bin

## Installing from Source Code
As Rebar3 is free, open source and written in Erlang, it's possible to simply clone and build it from the source code.

    $ git clone https://github.com/erlang/rebar3.git
    $ cd rebar3
    $ ./bootstrap

This will create the rebar3 script, which you can put on your PATH or link to `/usr/local/bin` as explained in the section "Installing Rebar3" above.

## Bootstrapping a new Erlang project
To bootstrap a new Erlang project, simply choose the template you want to use from the list. The available templates can be retrieved by the following command:

    $ rebar3 new

    app (built-in): Complete OTP Application structure
    cmake (built-in): Standalone Makefile for building C/C++ in c_src
    escript (built-in): Complete escriptized application structure
    lib (built-in): Complete OTP Library application (no processes) structure
    plugin (built-in): Rebar3 plugin project structure
    release (built-in): OTP Release structure for executable programs

Once you have chosen the appropriate template, bootstrap it with the following command (rebar3 will create a new directory for your project):

    $ rebar3 new lib libname

    ===> Writing libname/src/libname.erl
    ===> Writing libname/src/libname.app.src
    ===> Writing libname/rebar.config
    ===> Writing libname/.gitignore
    ===> Writing libname/LICENSE
    ===> Writing libname/README.md

OBS: Although you *can* run `rebar3 new <template> .` to create the new project in the current directory, this is not recommended, because the bootstrapped files will use `.` (dot) as application and module names and also in the `rebar.config`, which will cause you syntax problems.

## Definition
**Official page**: https://www.rebar3.org/

**Source code**: https://github.com/erlang/rebar3

Rebar3 is mainly a dependency manager for Erlang and Elixir projects, but it also offers several other features, like bootstrapping projects (according to several templates, following the OTP principles), task executor, build tool, test runner and is extensible by using plugins.

