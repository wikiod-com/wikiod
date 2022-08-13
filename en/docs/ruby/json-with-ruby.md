---
title: "JSON with Ruby"
slug: "json-with-ruby"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Using JSON with Ruby
JSON (JavaScript Object Notation) is a lightweight data interchange format.
Many web applications use it to send and receive data. 

In Ruby you can simply work with JSON. 

At first you have to `require 'json'`, then you can parse a JSON string via the `JSON.parse()` command.

    require 'json'
    
    j = '{"a": 1, "b": 2}'
    puts JSON.parse(j)
    >> {"a"=>1, "b"=>2}

What happens here, is that the parser generates a [Ruby Hash][1] out of the JSON.

The other way around, generating JSON out of a Ruby hash is as simple as parsing. The method of choice is `to_json`:

    require 'json'

    hash = { 'a' => 1, 'b' => 2 }
    json = hash.to_json
    puts json
    >> {"a":1,"b":2}

  [1]: https://www.wikiod.com/ruby/hashes


## Using Symbols
You can use JSON together with Ruby symbols.
With the option symbolize_names for the parser, the keys in the resulting hash will be symbols instead of strings.

    json = '{ "a": 1, "b": 2 }'
    puts JSON.parse(json, symbolize_names: true)
    >> {:a=>1, :b=>2}



