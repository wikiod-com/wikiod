---
title: "Behaviours"
slug: "behaviours"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Introduction
Behaviours are a list of functions specifications that another module can implement. They are similar to interfaces in other languages.

Here’s an example behaviour:

    defmodule Parser do
      @callback parse(String.t) :: any
      @callback extensions() :: [String.t]
    end

And a module that implements it:

    defmodule JSONParser do
      @behaviour Parser
    
      def parse(str), do: # ... parse JSON
      def extensions, do: ["json"]
    end

The `@behaviour` module attribute above indicates that this module is expected to define every function defined in the Parser module. Missing functions will result in undefined behaviour function compilation errors.

Modules can have multiple `@behaviour` attributes.

