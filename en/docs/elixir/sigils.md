---
title: "Sigils"
slug: "sigils"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Custom sigils
Custom sigils can be made by creating a method `sigil_X` where X is the letter you want to use (this can only be a single letter).

    defmodule Sigils do
      def sigil_j(string, options) do
        # Split on the letter p, or do something more useful
        String.split string, "p"
      end
      # Use this sigil in this module, or import it to use it elsewhere
    end

The `options` argument is a binary of the arguments given at the end of the sigil, for example:

    ~j/foople/abc # string is "foople", options are 'abc'
    # ["foo", "le"]

## Build a list of strings
    iex> ~w(a b c)
    ["a", "b", "c"]

## Build a list of atoms
    iex> ~w(a b c)a
    [:a, :b, :c]

