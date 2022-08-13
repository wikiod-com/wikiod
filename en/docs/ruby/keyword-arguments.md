---
title: "Keyword Arguments"
slug: "keyword-arguments"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

**Keyword arguments** were introduced in Ruby 2.0, and improved in Ruby 2.1 with the addition of _required_ keyword arguments.

A simple method with a keyword argument looks like the following one:

    def say(message: "Hello World")
      puts message
    end

    say
    # => "Hello World"

    say message: "Today is Monday"
    # => "Today is Monday"

As a reminder, the same method without keyword argument would have been:

    def say(message = "Hello World")
      puts message
    end

    say
    # => "Hello World"

    say "Today is Monday"
    # => "Today is Monday"

<!-- if version [lt 2.0] -->
You can simulate keyword argument in previous Ruby versions using a Hash parameter. This is still a very common practice, especially in libraries that provides compatibility with pre-2.0 Ruby versions:

    def say(options = {})
      message = options.fetch(:message, "Hello World")
      puts 
    end

    say
    # => "Hello World"

    say message: "Today is Monday"
    # => "Today is Monday"

<!-- end version if -->


## Using arbitrary keyword arguments with splat operator
You can define a method to accept an arbitrary number of keyword arguments using the _double splat_ (`**`) operator:

    def say(**args)
      puts args
    end

    say foo: "1", bar: "2"
    # {:foo=>"1", :bar=>"2"}

The arguments are captured in a `Hash`. You can manipulate the `Hash`, for example to extract the desired arguments.

    def say(**args)
      puts args[:message] || "Message not found"
    end

    say foo: "1", bar: "2", message: "Hello World"
    # Hello World

    say foo: "1", bar: "2"
    # Message not found

Using a the splat operator with keyword arguments will prevent keyword argument validation, the method will never raise an `ArgumentError` in case of unknown keyword.

As for the standard splat operator, you can re-convert a `Hash` into keyword arguments for a method:

    def say(message: nil, before: "<p>", after: "</p>")
      puts "#{before}#{message}#{after}"
    end

    args = { message: "Hello World", after: "</p><hr>" }
    say(**args)
    # <p>Hello World</p><hr>

    args = { message: "Hello World", foo: "1" }
    say(**args)
    # => ArgumentError: unknown keyword: foo

This is generally used when you need to manipulate incoming arguments, and pass them to an underlying method:

    def inner(foo:, bar:)
      puts foo, bar
    end

    def outer(something, foo: nil, bar: nil, baz: nil)
      puts something
      params = {}
      params[:foo] = foo || "Default foo"
      params[:bar] = bar || "Default bar"
      inner(**params)
    end

    outer "Hello:", foo: "Custom foo"
    # Hello:
    # Custom foo
    # Default bar


## Using keyword arguments
You define a keyword argument in a method by specifying the name in the method definition:

    def say(message: "Hello World")
      puts message
    end

    say
    # => "Hello World"

    say message: "Today is Monday"
    # => "Today is Monday"

You can define multiple keyword arguments, the definition order is irrelevant:

    def say(message: "Hello World", before: "<p>", after: "</p>")
      puts "#{before}#{message}#{after}"
    end

    say
    # => "<p>Hello World</p>"

    say message: "Today is Monday"
    # => "<p>Today is Monday</p>"

    say after: "</p><hr>", message: "Today is Monday"
    # => "<p>Today is Monday</p><hr>"

Keyword arguments can be mixed with positional arguments:

    def say(message, before: "<p>", after: "</p>")
      puts "#{before}#{message}#{after}"
    end

    say "Hello World", before: "<span>", after: "</span>"
    # => "<span>Hello World</span>"

Mixing keyword argument with positional argument was a very common approach before Ruby 2.1, because it was not possible to define [required keyword arguments](https://www.wikiod.com/ruby/keyword-arguments#Required keyword arguments).

Moreover, in Ruby < 2.0, it was very common to add an `Hash` at the end of a method definition to use for optional arguments. The syntax is very similar to keyword arguments, to the point where optional arguments via `Hash` are compatible with Ruby 2 keyword arguments.

    def say(message, options = {})
      before = option.fetch(:before, "<p>")
      after  = option.fetch(:after, "</p>")
      puts "#{before}#{message}#{after}"
    end

    # The method call is syntactically equivalent to the keyword argument one
    say "Hello World", before: "<span>", after: "</span>"
    # => "<span>Hello World</span>"

Note that trying to pass a not-defined keyword argument will result in an error:

    def say(message: "Hello World")
      puts message
    end

    say foo: "Hello"
    # => ArgumentError: unknown keyword: foo


## Required keyword arguments
<!-- if version [gte 2.1] -->
**Required keyword arguments** were introduced in Ruby 2.1, as an improvement to keyword arguments.

To define a keyword argument as required, simply declare the argument without a default value.

    def say(message:)
      puts message
    end

    say
    # => ArgumentError: missing keyword: message

    say message: "Hello World"
    # => "Hello World"

You can also mix required and non-required keyword arguments:

    def say(before: "<p>", message:, after: "</p>")
      puts "#{before}#{message}#{after}"
    end

    say
    # => ArgumentError: missing keyword: message

    say message: "Hello World"
    # => "<p>Hello World</p>"

    say message: "Hello World", before: "<span>", after: "</span>"
    # => "<span>Hello World</span>"

<!-- end version if -->

