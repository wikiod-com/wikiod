---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9883
type: docs
toc: true
---

## Syntax
 - using JSON
 - JSON.parse(str)
 - JSON.json(obj)
 - JSON.print(io, obj, indent)

Since neither Julia `Dict` nor JSON objects are inherently ordered, it's best not to rely on the order of key-value pairs in a JSON object.

## Installing JSON.jl
JSON is a popular data interchange format. The most popular JSON library for Julia is [JSON.jl](https://github.com/JuliaLang/JSON.jl). To install this package, use the package manager:

    julia> Pkg.add("JSON")

The next step is to test whether the package is working on your machine:

    julia> Pkg.test("JSON")

If all tests passed, then the library is ready for use.

## Parsing JSON
JSON that has been encoded as a string can easily be parsed into a standard Julia type:

    julia> using JSON

    julia> JSON.parse("""{
               "this": ["is", "json"],
               "numbers": [85, 16, 12.0],
               "and": [true, false, null]
           }""")
    Dict{String,Any} with 3 entries:
      "this"    => Any["is","json"]
      "numbers" => Any[85,16,12.0]
      "and"     => Any[true,false,nothing]


There are a few immediate properties of JSON.jl of note:

 - JSON types map to sensible types in Julia: Object becomes `Dict`, array becomes `Vector`, number becomes `Int64` or `Float64`, boolean becomes `Bool`, and null becomes `nothing::Void`.
 - JSON is an untyped container format: Thus returned Julia vectors are of type `Vector{Any}`, and returned dictionaries are of type `Dict{String, Any}`.
 - JSON standard does not distinguish between integers and decimal numbers, but JSON.jl does. A number without a decimal point or scientific notation is parsed into `Int64`, whereas a number with a decimal point is parsed into `Float64`. This matches closely with the behavior of JSON parsers in many other languages.

## Serializing JSON
The `JSON.json` function serializes a Julia object into a Julia `String` containing JSON:

    julia> using JSON

    julia> JSON.json(Dict(:a => :b, :c => [1, 2, 3.0], :d => nothing))
    "{\"c\":[1.0,2.0,3.0],\"a\":\"b\",\"d\":null}"

    julia> println(ans)
    {"c":[1.0,2.0,3.0],"a":"b","d":null}

If a string is not desired, JSON can be printed directly to an IO stream:

    julia> JSON.print(STDOUT, [1, 2, true, false, "x"])
    [1,2,true,false,"x"]

Note that `STDOUT` is the default, and can be omitted in the above call.

Prettier printing can be achieved by passing the optional `indent` parameter:

    julia> JSON.print(STDOUT, Dict(:a => :b, :c => :d), 4)
    {
        "c": "d",
        "a": "b"
    }

There is a sane default serialization for complex Julia types:

    julia> immutable Point3D
               x::Float64
               y::Float64
               z::Float64
           end

    julia> JSON.print(Point3D(1.0, 2.0, 3.0), 4)
    {
        "y": 2.0,
        "z": 3.0,
        "x": 1.0
    }



