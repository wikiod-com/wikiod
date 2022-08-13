---
title: "Lists"
slug: "lists"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Syntax
- []
- [1, 2, 3, 4]
- [1, 2] ++ [3, 4] # -> [1,2,3,4]
- hd([1, 2, 3, 4]) # -> 1
- tl([1, 2, 3, 4]) # -> [2,3,4]
- [head | tail]
- [1 | [2, 3, 4]] # -> [1,2,3,4]
- [1 | [2 | [3 | [4 | []]]]] -> [1,2,3,4]
- 'hello' = [?h, ?e, ?l, ?l, ?o]
- keyword_list = [a: 123, b: 456, c: 789]
- keyword_list[:a] # -> 123

## Keyword Lists
Keyword lists are lists where each item in the list is a tuple of an atom followed by a value.

    keyword_list = [{:a, 123}, {:b, 456}, {:c, 789}]

A shorthand notation for writing keyword lists is as follows:

    keyword_list = [a: 123, b: 456, c: 789]

Keyword lists are useful for creating ordered key-value pair data structures, where multiple items can exist for a given key.

The first item in a keyword list for a given key can be obtained like so:

    iex> keyword_list[:b]
    456

A use case for keyword lists could be a sequence of named tasks to run:

    defmodule TaskRunner do
      def run_tasks(tasks) do
        # Call a function for each item in the keyword list.
        # Use pattern matching on each {:key, value} tuple in the keyword list
        Enum.each(tasks, fn
          {:delete, x} ->
            IO.puts("Deleting record " <> to_string(x) <> "...")
          {:add, value} ->
            IO.puts("Adding record \"" <> value <> "\"...")
          {:update, {x, value}} ->
            IO.puts("Setting record " <> to_string(x) <> " to \"" <> value <> "\"...")
        end)
      end
    end
    
This code can be called with a keyword list like so:

    iex> tasks = [
    ...>   add: "foo",
    ...>   add: "bar",
    ...>   add: "test",
    ...>   delete: 2,
    ...>   update: {1, "asdf"}
    ...> ]

    iex> TaskRunner.run_tasks(tasks)
    Adding record "foo"...
    Adding record "bar"...
    Adding record "test"...
    Deleting record 2...
    Setting record 1 to "asdf"...


## Char Lists
Strings in Elixir are "binaries".  However, in Erlang code, strings are traditionally "char lists", so when calling Erlang functions, you may have to use char lists instead of regular Elixir strings.

While regular strings are written using double quotes `"`, char lists are written using single quotes `'`:

    string = "Hello!"
    char_list = 'Hello!'

Char lists are simply lists of integers representing the code points of each character.

    'hello' = [104, 101, 108, 108, 111]

A string can be converted to a char list with [`to_charlist/1`](http://elixir-lang.org/docs/stable/elixir/Kernel.html#to_charlist/1):

    iex> to_charlist("hello")
    'hello'

And the reverse can be done with [`to_string/1`](http://elixir-lang.org/docs/stable/elixir/Kernel.html#to_string/1):

    iex> to_string('hello')
    "hello"

Calling an Erlang function and converting the output to a regular Elixir string:

    iex> :os.getenv |> hd |> to_string
    "PATH=/usr/local/bin:/usr/bin:/bin"

## Cons Cells
Lists in Elixir are linked lists.  This means that each item in a list consists of a value, followed by a pointer to the next item in the list.  This is implemented in Elixir using cons cells.

Cons cells are simple data structures with a "left" and a "right" value, or a "head" and a "tail".

A `|` symbol can be added before the last item in a list to notate an (improper) list with a given head and tail.  The following is a single cons cell with `1` as the head and `2` as the tail:

    [1 | 2]

The standard Elixir syntax for a list is actually equivalent to writing a chain of nested cons cells:

    [1, 2, 3, 4] = [1 | [2 | [3 | [4 | []]]]]

The empty list `[]` is used as the tail of a cons cell to represent the end of a list.

All lists in Elixir are equivalent to the form `[head | tail]`, where `head` is the first item of the list and `tail` is the rest of the list, minus the head.

    iex> [head | tail] = [1, 2, 3, 4]
    [1, 2, 3, 4]
    iex> head
    1
    iex> tail
    [2, 3, 4]

Using the `[head | tail]` notation is useful for pattern matching in recursive functions:

    def sum([]), do: 0
    
    def sum([head | tail]) do
      head + sum(tail)
    end

## Mapping Lists
`map` is a function in functional programming which given a list and a function, returns a new list with the function applied to each item in that list.  In Elixir, the [`map/2`](http://elixir-lang.org/docs/stable/elixir/Enum.html#map/2) function is in the [Enum](http://elixir-lang.org/docs/stable/elixir/Enum.html) module.

    iex> Enum.map([1, 2, 3, 4], fn(x) -> x + 1 end)
    [2, 3, 4, 5]

Using the alternative capture syntax for anonymous functions:

    iex> Enum.map([1, 2, 3, 4], &(&1 + 1))
    [2, 3, 4, 5]

Referring to a function with capture syntax:

    iex> Enum.map([1, 2, 3, 4], &to_string/1)
    ["1", "2", "3", "4"]

Chaining list operations using the pipe operator:

    iex> [1, 2, 3, 4]
    ...> |> Enum.map(&to_string/1)
    ...> |> Enum.map(&("Chapter " <> &1))
    ["Chapter 1", "Chapter 2", "Chapter 3", "Chapter 4"]

## List Comprehensions
Elixir doesn't have loops. Instead of them for lists there are great `Enum` and `List` modules, but there are also List Comprehensions.

List Comprehensions can be useful to:
- create new lists


    iex(1)> for value <- [1, 2, 3], do: value + 1
    [2, 3, 4] 

* filtering lists, using `guard` expressions but you use them without `when` keyword.


    iex(2)> odd? = fn x -> rem(x, 2) == 1 end
    iex(3)> for value <- [1, 2, 3], odd?.(value), do: value
    [1, 3]

* create custom map, using `into` keyword:

    
    iex(4)> for value <- [1, 2, 3], into: %{}, do: {value, value + 1}
    %{1 => 2, 2=>3, 3 => 4}

# Combined example

     
    iex(5)> for value <- [1, 2, 3], odd?.(value), into: %{}, do: {value, value * value}
    %{1 => 1, 3 => 9}

# Summary
List Comprehensions:
- uses `for..do` syntax with additional guards after commas and `into` keyword when returning other structure than lists ie. map.
- in other cases return new lists
- doesn't support accumulators
- can't stop processing when certain condition is met
- `guard` statements have to be first in order after `for` and before `do` or `into` symbols. Order of symbols doesn't matter

According to these constraints List Comprehensions are limited only for simple usage. In  more advanced cases using functions from `Enum` and `List` modules would be the best idea. 

    



## List difference
    iex> [1, 2, 3] -- [1, 3]
    [2]

`--` removes the first occurrence of an item on the left list for each item on the
right.


## List Membership
Use `in` operator to check if an element is a member of a list.

    iex> 2 in [1, 2, 3]
    true
    iex> "bob" in [1, 2, 3]
    false

## Converting Lists to a Map
Use `Enum.chunk/2` to group elements into sub-lists, and `Map.new/2` to convert it into a Map:

    [1, 2, 3, 4, 5, 6]
    |> Enum.chunk(2)
    |> Map.new(fn [k, v] -> {k, v} end)

Would give:

    %{1 => 2, 3 => 4, 5 => 6}

