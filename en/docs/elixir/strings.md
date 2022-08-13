---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

A `String` in Elixir is a `UTF-8` encoded binary.

## Convert to string
Use Kernel.inspect to convert anything to string.

    iex> Kernel.inspect(1)
    "1"
    iex> Kernel.inspect(4.2)
    "4.2"
    iex> Kernel.inspect %{pi: 3.14, name: "Yos"}
    "%{pi: 3.14, name: \"Yos\"}"

## Get a substring
    iex> my_string = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    iex> String.slice my_string, 6..10
    "ipsum"

## Split a string
    iex> String.split("Elixir, Antidote, Panacea", ",")
    ["Elixir", "Antidote", "Panacea"]

## String Interpolation
```
iex(1)> name = "John"
"John"
iex(2)> greeting = "Hello, #{name}"
"Hello, John"
iex(3)> num = 15
15
iex(4)> results = "#{num} item(s) found."
"15 item(s) found."
```

## Check if String contains Substring
    iex(1)> String.contains? "elixir of life", "of"
    true
    iex(2)> String.contains? "elixir of life", ["life", "death"]
    true
    iex(3)> String.contains? "elixir of life", ["venus", "mercury"]
    false

## Join Strings
You can concatenate strings in Elixir using the `<>` operator:

    "Hello" <> "World"                           # => "HelloWorld"

---

For a `List` of Strings, you can use `Enum.join/2`:

    Enum.join(["A", "few", "words"], " ")        # => "A few words"




