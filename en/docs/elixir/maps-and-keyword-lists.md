---
title: "Maps and Keyword Lists"
slug: "maps-and-keyword-lists"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- map = %{} // creates an empty map
- map = %{:a => 1, :b => 2} // creates a non-empty map
- list = [] // creates an empty list
- list = [{:a, 1}, {:b, 2}] // creates a non-empty keyword list


Elixir provides two associative data structures: _maps_ and _keyword lists_.

_Maps_ are the Elixir key-value (also called dictionary or hash in other languages) type.

_Keyword lists_ are tuples of key/value that associate a value to a certain key. They are generally used as options for a function call.


## Creating a Map
Maps are the Elixir key-value (also called dictionary or hash in other languages) type. You create a map using the `%w{}` syntax:

    %{} // creates an empty map
    %{:a => 1, :b => 2} // creates a non-empty map

Keys and values can use be any type:

    %{"a" => 1, "b" => 2}
    %{1 => "a", 2 => "b"}

Moreover, you can have maps with mixed types for both keys and values":

    // keys are integer or strings
    %{1 => "a", "b" => :foo}
    // values are string or nil
    %{1 => "a", 2 => nil}

When all the keys in a map are atoms, you can use the keyword syntax for convenience:

    %{a: 1, b: 2}

## Creating a Keyword List
Keyword lists are tuples of key/value, generally used as options for a function call.

    [{:a, 1}, {:b, 2}] // creates a non-empty keyword list

Keyword lists can have the same key repeated more than once.

    [{:a, 1}, {:a, 2}, {:b, 2}]
    [{:a, 1}, {:b, 2}, {:a, 2}]

Keys and values can be any type:

    [{"a", 1}, {:a, 2}, {2, "b"}]


## Difference between Maps and Keyword Lists
Maps and keyword lists have different application. For instance, a map cannot have two keys with the same value and it's not ordered. Conversely, a Keyword list can be a little bit hard to use in pattern matching in some cases.

Here's a few use cases for maps vs keyword lists.

Use keyword lists when:
- you need the elements to be ordered
- you need more than one element with the same key

Use maps when:
- you want to pattern-match against some keys/values
- you don't need more than one element with the same key
- whenever you don't explicitly need a keyword list


