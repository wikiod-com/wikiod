---
title: "while Loops"
slug: "while-loops"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Syntax
- while cond; body; end
- break
- continue

The `while` loop does not have a value; although it can be used in expression position, its type is `Void` and the value obtained will be `nothing`.

## Collatz sequence
The `while` loop runs its body as long as the condition holds. For instance, the following code computes and prints the [Collatz sequence](https://en.wikipedia.org/wiki/Collatz_conjecture) from a given number:

    function collatz(n)
        while n ≠ 1
            println(n)
            n = iseven(n) ? n ÷ 2 : 3n + 1
        end
        println("1... and 4, 2, 1, 4, 2, 1 and so on")
    end

Usage:

    julia> collatz(10)
    10
    5
    16
    8
    4
    2
    1... and 4, 2, 1, 4, 2, 1 and so on

It is possible to write any loop recursively, and for complex `while` loops, sometimes the recursive variant is more clear. However, in Julia, loops have some distinct advantages over recursion:

- Julia does not guarantee tail call elimination, so recursion uses additional memory and may cause stack overflow errors.
- And further, for the same reason, a loop can have decreased overhead and run faster.

## Breadth-first search
<!-- if version [gte 0.5.0] -->

(Although this example is written using syntax introduced in version v0.5, it can work with few modifications on older versions also.)

This implementation of [breadth-first search](https://en.wikipedia.org/wiki/Breadth-first_search) (BFS) on a graph represented with adjacency lists uses `while` loops and the `return` statement. The task we will solve is as follows: we have a sequence of people, and a sequence of friendships (friendships are mutual). We want to determine the degree of the connection between two people. That is, if two people are friends, we will return `1`; if one is a friend of a friend of the other, we will return `2`, and so on.

First, let’s assume we already have an adjacency list: a `Dict` mapping `T` to `Array{T, 1}`, where the keys are people and the values are all the friends of that person. Here we can represent people with whatever type `T` we choose; in this example, we will use `Symbol`. In the BFS algorithm, we keep a queue of people to “visit”, and mark their distance from the origin node.

    function degree(adjlist, source, dest)
        distances = Dict(source => 0)
        queue = [source]

        # until the queue is empty, get elements and inspect their neighbours
        while !isempty(queue)
            # shift the first element off the queue
            current = shift!(queue)

            # base case: if this is the destination, just return the distance
            if current == dest
                return distances[dest]
            end

            # go through all the neighbours
            for neighbour in adjlist[current]
                # if their distance is not already known...
                if !haskey(distances, neighbour)
                    # then set the distance
                    distances[neighbour] = distances[current] + 1

                    # and put into queue for later inspection
                    push!(queue, neighbour)
                end
            end
        end

        # we could not find a valid path
        error("$source and $dest are not connected.")
    end

Now, we will write a function to build an adjacency list given a sequence of people, and a sequence of `(person, person)` tuples:

    function makeadjlist(people, friendships)
        # dictionary comprehension (with generator expression)
        result = Dict(p => eltype(people)[] for p in people)

        # deconstructing for; friendship is mutual
        for (a, b) in friendships
            push!(result[a], b)
            push!(result[b], a)
        end

        result
    end

We can now define the original function:

    degree(people, friendships, source, dest) =
        degree(makeadjlist(people, friendships), source, dest)

Now let’s test our function on some data.

    const people = [:jean, :javert, :cosette, :gavroche, :éponine, :marius]
    const friendships = [
        (:jean, :cosette),
        (:jean, :marius),
        (:cosette, :éponine),
        (:cosette, :marius),
        (:gavroche, :éponine)
    ]

Jean is connected to himself in `0` steps:

    julia> degree(people, friendships, :jean, :jean)
    0

Jean and Cosette are friends, and so have degree `1`:

    julia> degree(people, friendships, :jean, :cosette)
    1

Jean and Gavroche are connected indirectly through Cosette and then Marius, so their degree is `3`:

    julia> degree(people, friendships, :jean, :gavroche)
    3

Javert and Marius are not connected through any chain, so an error is raised:

    julia> degree(people, friendships, :javert, :marius)
    ERROR: javert and marius are not connected.
     in degree(::Dict{Symbol,Array{Symbol,1}}, ::Symbol, ::Symbol) at ./REPL[28]:27
     in degree(::Array{Symbol,1}, ::Array{Tuple{Symbol,Symbol},1}, ::Symbol, ::Symbol) at ./REPL[30]:1

<!-- end version if -->

## Run once before testing condition
Sometimes, one wants to run some initialization code once before testing a condition. In certain other languages, this kind of loop has special `do`-`while` syntax. However, this syntax can be replaced with a regular `while` loop and `break` statement, so Julia does not have specialized `do`-`while` syntax. Instead, one writes:

    local name

    # continue asking for input until satisfied
    while true
        # read user input
        println("Type your name, without lowercase letters:")
        name = readline()

        # if there are no lowercase letters, we have our result!
        !any(islower, name) && break
    end

Note that in some situations, such loops could be more clear with recursion:

    function getname()
        println("Type your name, without lowercase letters:")
        name = readline()
        if any(islower, name)
            getname()  # this name is unacceptable; try again
        else
            name       # this name is good, return it
        end
    end

