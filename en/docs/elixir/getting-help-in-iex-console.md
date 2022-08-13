---
title: "Getting help in IEx console"
slug: "getting-help-in-iex-console"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

IEx provides access to Elixir documentation. When Elixir is installed on your system you can start IEx e.g. with `iex` command in a terminal. Then type `h` command on IEx command line followed by the function name prepended by its module name e.g. `h List.foldr` 

## Listing Elixir modules and functions
To get the list of Elixir modules just type

    h Elixir.[TAB]

Pressing [TAB] autocompletes modules and functions names. In this case it lists all modules. To find all functions in a module e.g. `List` use

    h List.[TAB]

