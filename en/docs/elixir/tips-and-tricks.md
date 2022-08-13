---
title: "Tips and Tricks"
slug: "tips-and-tricks"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Elixir Advanced tips and tricks which save our time while coding.

## Creating Custom Sigils and Documenting
Each x sigil call respective sigil_x definition

Defining Custom Sigils
```elxiir
defmodule MySigils do
  #returns the downcasing string if option l is given then returns the list of downcase letters
  def sigil_l(string,[]), do: String.Casing.downcase(string)
  def sigil_l(string,[?l]), do: String.Casing.downcase(string) |> String.graphemes
  
  #returns the upcasing string if option l is given then returns the list of downcase letters
  def sigil_u(string,[]), do: String.Casing.upcase(string)
  def sigil_u(string,[?l]), do: String.Casing.upcase(string) |> String.graphemes
end
```

## Multiple [ OR ]
This is just the other way of writing Multiple OR conditions. This is not the recommended approach because in regular approach when the condition evaluates to true, it stops executing the remaining conditions which save the time of evaluation, unlike this approach which evaluates all conditions first in the list. This is just bad but good for discoveries.

```elixir
# Regular Approach
find = fn(x) when x>10 or x<5 or x==7 -> x end 

# Our Hack
hell = fn(x) when true in [x>10,x<5,x==7] -> x end
```

## iex Custom Configuration - iex Decoration
Copy the content into a file and save the file as .iex.exs in your ~ home directory and see the magic. You can also download the file [HERE](https://gist.github.com/blackode/5728517116d7a4d08f0a4faddd8c145a)
```elixir
# IEx.configure colors: [enabled: true]
# IEx.configure colors: [ eval_result: [ :cyan, :bright ] ]
IO.puts IO.ANSI.red_background() <> IO.ANSI.white() <> " ❄❄❄ Good Luck with Elixir ❄❄❄ " <> IO.ANSI.reset
Application.put_env(:elixir, :ansi_enabled, true)
IEx.configure(
 colors: [
   eval_result: [:green, :bright] ,
   eval_error: [[:red,:bright,"Bug Bug ..!!"]],
   eval_info: [:yellow, :bright ],
 ],
 default_prompt: [
   "\e[G",    # ANSI CHA, move cursor to column 1
    :white,
    "I",
    :red,
    "❤" ,       # plain string
    :green,
    "%prefix",:white,"|",
     :blue,
     "%counter",
     :white,
     "|",
    :red,
    "▶" ,         # plain string
    :white,
    "▶▶"  ,       # plain string
      # ❤ ❤-»" ,  # plain string
    :reset
  ] |> IO.ANSI.format |> IO.chardata_to_string

)
```

