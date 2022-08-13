---
title: "instance_eval"
slug: "instance_eval"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
- object.instance_eval 'code'
- object.instance_eval 'code', 'filename'
- object.instance_eval 'code', 'filename', 'line number'
- object.instance_eval { code }
- object.instance_eval { |receiver| code }

## Parameters
| Parameter  | Details                                                   |
| ---------- | --------------------------------------------------------- |
| `string`   | Contains the Ruby source code to be evaluated.            |
| `filename` | File name to use for error reporting.                     |
| `lineno`   | Line number to use for error reporting.                   |
| `block`    | The block of code to be evaluated.                        |
| `obj`      | The receiver is passed to the block as its only argument. |

## Instance evaluation
The [`instance_eval`][BasicObject#instance_eval] method is available on all objects. It evaluates code in the context of the receiver:

    object = Object.new

    object.instance_eval do
      @variable = :value
    end

    object.instance_variable_get :@variable # => :value

`instance_eval` sets `self` to `object` for the duration of the code block:
 
    object.instance_eval { self == object } # => true
 
The receiver is also passed to the block as its only argument:
 
    object.instance_eval { |argument| argument == object } # => true
 
The [`instance_exec`][BasicObject#instance_exec] method differs in this regard: it passes its arguments to the block instead.

    object.instance_exec :@variable do |name|
      instance_variable_get name # => :value
    end

[BasicObject#instance_eval]: http://ruby-doc.org/core/BasicObject.html#method-i-instance_eval
[BasicObject#instance_exec]: http://ruby-doc.org/core/BasicObject.html#method-i-instance_exec

## Implementing with
Many languages feature a `with` statement that allows programmers to omit the receiver of method calls.

`with` can be easily emulated in Ruby using [`instance_eval`][BasicObject#instance_eval]:

    def with(object, &block)
      object.instance_eval &block
    end

The `with` method can be used to seamlessly execute methods on objects:

    hash = Hash.new

    with hash do
      store :key, :value
      has_key? :key       # => true
      values              # => [:value]
    end

[BasicObject#instance_eval]: http://ruby-doc.org/core/BasicObject.html#method-i-instance_eval

