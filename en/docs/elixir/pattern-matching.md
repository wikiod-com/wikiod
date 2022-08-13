---
title: "Pattern matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

## Pattern matching on a list
You can also pattern match on Elixir Data Structures such as Lists.

**Lists**

Matching on a list is quite simple.

```elixir
[head | tail] = [1,2,3,4,5]
# head == 1
# tail == [2,3,4,5]
```

This works by matching the first (or more) elements in the list to the left hand side of the `|` (pipe) and the rest of the list to the right hand side variable of the `|`.

We can also match on specific values of a list:

```elixir
[1,2 | tail] = [1,2,3,4,5]
# tail = [3,4,5]

[4 | tail] = [1,2,3,4,5]
** (MatchError) no match of right hand side value: [1, 2, 3, 4, 5]
```
Binding multiple consecutive values on the left of the `|` is also allowed:
```
[a, b | tail] = [1,2,3,4,5]
# a == 1
# b == 2
# tail = [3,4,5]
```

Even more complex - we can match on a specific value, and match that against a variable:

```elixir
iex(11)> [a = 1 | tail] = [1,2,3,4,5]
# a == 1
```


## Pattern matching on a map
```elixir
%{username: username} = %{username: "John Doe", id: 1}
# username == "John Doe"
```

```elixir
%{username: username, id: 2} = %{username: "John Doe", id: 1}
** (MatchError) no match of right hand side value: %{id: 1, username: "John Doe"}

## Tuples
    { a, b, c } = { "Hello", "World", "!" }    

    IO.puts a # Hello
    IO.puts b # World
    IO.puts c # !

    # Tuples of different size won't match:

    { a, b, c } = { "Hello", "World" } # (MatchError) no match of right hand side value: { "Hello", "World" }

## Reading a File

Pattern matching is useful for an operation like file reading which returns a tuple.

If the file `sample.txt` contains `This is  a sample text`, then: 

    { :ok, file } = File.read("sample.txt")
    # => {:ok, "This is a sample text"}

    file
    # => "This is a sample text"

Otherwise, if the file does not exist:    

    { :ok, file } = File.read("sample.txt")
    # => ** (MatchError) no match of right hand side value: {:error, :enoent}

    { :error, msg } = File.read("sample.txt")
    # => {:error, :enoent}
        

## Pattern matching functions
<!-- language: default -->
    #You can use pattern matching to run different 
    #functions based on which parameters you pass
    
    #This example uses pattern matching to start, 
    #run, and end a recursive function

    defmodule Counter do
        def count_to do
            count_to(100, 0) #No argument, init with 100
        end

        def count_to(counter) do
            count_to(counter, 0) #Initialize the recursive function
        end

        def count_to(counter, value) when value == counter do
            #This guard clause allows me to check my arguments against
            #expressions. This ends the recursion when the value matches
            #the number I am counting to.
            :ok
        end

        def count_to(counter, value) do
            #Actually do the counting
            IO.puts value
            count_to(counter, value + 1)
        end
    end

## Get the sum of a list using pattern matching
```
defmodule Math do
  # We start of by passing the sum/1 function a list of numbers.
  def sum(numbers) do
    do_sum(numbers, 0)
  end

  # Recurse over the list when it contains at least one element.
  # We break the list up into two parts:
  #   head: the first element of the list
  #   tail: a list of all elements except the head
  # Every time this function is executed it makes the list of numbers
  # one element smaller until it is empty.
  defp do_sum([head|tail], acc) do
    do_sum(tail, head + acc)
  end

 # When we have reached the end of the list, return the accumulated sum
  defp do_sum([], acc), do: acc
end
```

## Anonymous functions
    f = fn
      {:a, :b} -> IO.puts "Tuple {:a, :b}"
      [] -> IO.puts "Empty list"
    end
    
    f.({:a, :b}) # Tuple {:a, :b}
    f.([])       # Empty list

## Pattern matching anonymous functions
```elixir
fizzbuzz = fn
  (0, 0, _) -> "FizzBuzz"
  (0, _, _) -> "Fizz"
  (_, 0, _) -> "Buzz"
  (_, _, x) -> x
end

my_function = fn(n) ->
  fizzbuzz.(rem(n, 3), rem(n, 5), n)
end
```


