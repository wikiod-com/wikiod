---
title: "Dictionaries"
slug: "dictionaries"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Using Dictionaries
Dictionaries can be constructed by passing it any number of pairs.

    julia> Dict("A"=>1, "B"=>2)
    Dict{String,Int64} with 2 entries:
      "B" => 2
      "A" => 1

You can get entries in a dictionary putting the key in square brackets.

    julia> dict = Dict("A"=>1, "B"=>2)
    Dict{String,Int64} with 2 entries:
      "B" => 2
      "A" => 1

    julia> dict["A"]
    1


