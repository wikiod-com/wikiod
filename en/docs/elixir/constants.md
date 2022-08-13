---
title: "Constants"
slug: "constants"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

So this is a summary analysis I've done based on the methods listed at http://stackoverflow.com/questions/33851536/how-do-you-define-constants-in-elixir-modules. I'm posting it for a couple reasons:

 - Most Elixir documentation is quite thorough, but I found this key architectural decision lacking guidance - so I would have requested it as a topic.
 - I wanted to get a little visibility and comments from others about the topic.
 - I also wanted to test out the new SO Documentation workflow. ;)

I've also uploaded the entire code to the GitHub repo [elixir-constants-concept](https://github.com/bill-mybiz/elixir-constants-concept).

## Module-scoped constants
    defmodule MyModule do
      @my_favorite_number 13
      @use_snake_case "This is a string (use double-quotes)"
    end

These are only accessible from within this module.

## Constants as functions
Declare:

    defmodule MyApp.ViaFunctions.Constants do
      def app_version, do: "0.0.1"
      def app_author, do: "Felix Orr"
      def app_info, do: [app_version, app_author]
      def bar, do: "barrific constant in function"
    end

Consume with require:

    defmodule MyApp.ViaFunctions.ConsumeWithRequire do
      require MyApp.ViaFunctions.Constants
    
      def foo() do
        IO.puts MyApp.ViaFunctions.Constants.app_version
        IO.puts MyApp.ViaFunctions.Constants.app_author
        IO.puts inspect MyApp.ViaFunctions.Constants.app_info
      end
    
      # This generates a compiler error, cannot invoke `bar/0` inside a guard.
      # def foo(_bar) when is_bitstring(bar) do
      #   IO.puts "We just used bar in a guard: #{bar}"
      # end
    end

Consume with import:

    defmodule MyApp.ViaFunctions.ConsumeWithImport do
      import MyApp.ViaFunctions.Constants
    
      def foo() do
        IO.puts app_version
        IO.puts app_author
        IO.puts inspect app_info
      end
    end

This method allows for reuse of constants across projects, but they will not be usable within guard functions that require compile-time constants.

## Constants via macros
Declare:

    defmodule MyApp.ViaMacros.Constants do
      @moduledoc """
      Apply with `use MyApp.ViaMacros.Constants, :app` or `import MyApp.ViaMacros.Constants, :app`.
    
      Each constant is private to avoid ambiguity when importing multiple modules
      that each have their own copies of these constants.
      """
    
      def app do
        quote do
          # This method allows sharing module constants which can be used in guards.
          @bar "barrific module constant"
          defp app_version, do: "0.0.1"
          defp app_author, do: "Felix Orr"
          defp app_info, do: [app_version, app_author]
        end
      end
    
      defmacro __using__(which) when is_atom(which) do
        apply(__MODULE__, which, [])
      end
    end

Consume with `use`:

    defmodule MyApp.ViaMacros.ConsumeWithUse do
      use MyApp.ViaMacros.Constants, :app
    
      def foo() do
        IO.puts app_version
        IO.puts app_author
        IO.puts inspect app_info
      end
    
      def foo(_bar) when is_bitstring(@bar) do
        IO.puts "We just used bar in a guard: #{@bar}"
      end
    end

This method allows you to use the `@some_constant` inside guards. I'm not even sure that the functions would be strictly necessary. 
    

