---
title: "Struct"
slug: "struct"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- Structure = Struct.new :attribute

## Creating new structures for data
[`Struct`][Struct] defines new classes with the specified attributes and accessor methods.

    Person = Struct.new :first_name, :last_name

You can then instantiate objects and use them:

    person = Person.new 'John', 'Doe'
    # => #<struct Person first_name="John", last_name="Doe">

    person.first_name
    # => "John"

    person.last_name
    # => "Doe"

[Struct]: http://ruby-doc.org/core/Struct.html

## Customizing a structure class
    Person = Struct.new :name do
      def greet(someone)
        "Hello #{someone}! I am #{name}!"
      end
    end

    Person.new('Alice').greet 'Bob'
    # => "Hello Bob! I am Alice!"

## Attribute lookup
Attributes can be accessed strings and symbols as keys. Numerical indexes also work.

    Person = Struct.new :name
    alice = Person.new 'Alice'

    alice['name']  # => "Alice"
    alice[:name]   # => "Alice"
    alice[0]       # => "Alice"

