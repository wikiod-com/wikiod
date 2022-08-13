---
title: "Mix"
slug: "mix"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Create a Custom Mix Task
    # lib/mix/tasks/mytask.ex
    defmodule Mix.Tasks.MyTask do
      use Mix.Task
    
      @shortdoc "A simple mix task"
      def run(_) do
        IO.puts "YO!"
      end
    end

Compile and run:

    $ mix compile
    $ mix my_task
    "YO!"

## Aliases
Elixir allows you to add aliases for your mix commands. Cool thing if you want to save yourself some typing.

Open `mix.exs` in your Elixir project.

First, add `aliases/0` function to the keyword list that the `project` function returns.
*Adding `()` at the end of the aliases function will prevent compiler from throwing a warning.*
```
  def project do
    [app: :my_app,
     ...
     aliases: aliases()]
  end
```

Then, define your `aliases/0` function (e.g. at the bottom of your `mix.exs` file).
```
  ...

  defp aliases do
    [go: "phoenix.server",
     trident: "do deps.get, compile, go"]
  end
```
You can now use `$ mix go` to run your Phoenix server (if you're running a [Phoenix][1] application). And use `$ mix trident` to tell mix to fetch all dependencies, compile, and run the server.


  [1]: http://www.phoenixframework.org/

## Custom mix task with command line arguments
In a basic implementation the task module must define a `run/1` function that takes a list of arguments. E.g. `def run(args) do ... end`

    defmodule Mix.Tasks.Example_Task do
      use Mix.Task
    
      @shortdoc "Example_Task prints hello + its arguments"
      def run(args) do
        IO.puts "Hello #{args}"
      end
    end

Compile and run:
    
    $ mix example_task world
    "hello world"

## Get help on available mix tasks
To list available mix tasks use:

    mix help
To get help on a specific task use `mix help task` e.g.:

    mix help cmd

