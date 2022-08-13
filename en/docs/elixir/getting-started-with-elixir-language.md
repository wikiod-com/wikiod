---
title: "Getting started with Elixir Language"
slug: "getting-started-with-elixir-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
For installation instructions on elixir check [here][1], it describes instructions related to different platforms.  

Elixir is a programming language that is created using `erlang`, and uses erlang's `BEAM` runtime (like `JVM` for java).  

We can use elixir in two modes: interactive shell `iex` or directly running using
`elixir` command.

Place the following in a file named `hello.exs`:

    IO.puts "Hello world!"

From the command line, type the following command to execute the Elixir source file:

    $ elixir hello.exs

This should output:

> Hello world!

This is known as the _scripted mode_ of `Elixir`. In fact, Elixir programs can also be compiled (and generally, they are) into bytecode for the BEAM virtual machine.

You can also use `iex` for interactive elixir shell (recommended), run the command
you will get a prompt like this:

    Interactive Elixir (1.3.4) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)>

Here you can try your elixir `hello world` examples:

    iex(1)> IO.puts "hello, world"
    hello, world
    :ok
    iex(2)> 

  [1]: http://elixir-lang.org/install.html

You can also compile and run your modules through `iex`. For example, if you have a `helloworld.ex` that contains:

    defmodule Hello do
       def sample do
           IO.puts "Hello World!"
       end
    end

Through `iex`, do: 

    iex(1)> c("helloworld.ex")
    [Hello]
    iex(2)> Hello.sample
    Hello World!

## Hello World from IEx
You can also use the `IEx` (Interactive Elixir) shell to evaluate expressions and execute code.

If you are on Linux or Mac, just type iex on your bash and press enter:

    $ iex

If you are on a Windows machine, type:

    C:\ iex.bat

Then you will enter into the IEx REPL (Read, Evaluate, Print, Loop), and you can just type something like:

    iex(1)> "Hello World"
    "Hello World"

If you want to load a script while opening an IEx REPL, you can do this:

    $ iex script.exs

Given `script.exs` is your script. You can now call functions from the script in the console.

  [1]: https://www.wikiod.com/elixir/getting-started-with-elixir-language


