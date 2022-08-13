---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

The `json` gem for Ruby allows for the parsing and creation of JSON.

## Syntax
- JSON.parse(json_document_string) => returns a Hash of the JSON document
- JSON.generate(ruby_hash) => returns a JSON document in the form of a String

## Parameters
| Parameter | Details |
| ------ | ------ |
| json_document_string | A JSON document in the form of a String |
| ruby_hash | Any Hash object | 

## Hash to JSON
    require 'json'
    data = {"test" => 123}
    puts JSON.generate(data)

## JSON to Hash
    require 'json'
    document = "{\"test\":123}"
    puts JSON.parse(document)

## Alternate JSON to Hash
    require 'json'
    data = JSON '{"test":23}'  # => {"test"=>23}

or

    require 'json'
    data = JSON['{"test":23}'] # => {"test"=>23}

## Alternate Hash to JSON
    require 'json'
    document = JSON 'test'  => 23 # => "{\"test\":23}"

or

    require 'json'
    document = JSON['test' => 23] # => "{\"test\":23}"

