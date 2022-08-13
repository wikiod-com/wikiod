---
title: "Enumerators"
slug: "enumerators"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

An [`Enumerator`][enumerator] is an object that implements iteration in a controlled fashion.

Instead of looping until some condition is satisfied, the object _enumerates_ values as needed. Execution of the loop is paused until the next value is requested by the owner of the object.

Enumerators make infinite streams of values possible.

  [enumerator]: http://ruby-doc.org/core/Enumerator.html

## Parameters
| Parameter | Details |
| --------- | ------- |
| `yield`   |  Responds to `yield`, which is aliased as `<<`. Yielding to this object implements iteration. |

## Custom enumerators
Let's create an [`Enumerator`][Enumerator] for [Fibonacci numbers].

    fibonacci = Enumerator.new do |yielder|
      a = b = 1
      loop do
        yielder << a
        a, b = b, a + b
      end
    end

We can now use any [`Enumerable`][Enumerable] method with `fibonacci `:

    fibonacci.take 10
    # => [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

[Fibonacci numbers]: https://en.wikipedia.org/wiki/Fibonacci_number
[Enumerator]: http://ruby-doc.org/core/Enumerator.html
[Enumerable]: http://ruby-doc.org/core/Enumerable.html

## Existing methods
If an iteration method such as `each` is called without a block, an [`Enumerator`][Enumerator] should be returned.

This can be done using the [`enum_for`][Object.enum_for] method:

    def each
      return enum_for :each unless block_given?
    
      yield :x
      yield :y
      yield :z
    end

This enables the programmer to compose [`Enumerable`][Enumerable] operations:

    each.drop(2).map(&:upcase).first
    # => :Z

[Enumerator]: http://ruby-doc.org/core/Enumerator.html
[Object.enum_for]: http://ruby-doc.org/core/Object.html#method-i-enum_for
[Enumerable]: http://ruby-doc.org/core/Enumerable.html

## Rewinding
Use [`rewind`][Enumerator.rewind] to restart the enumerator.

    ℕ = Enumerator.new do |yielder|
      x = 0
      loop do
        yielder << x
        x += 1
      end
    end
    
    ℕ.next
    # => 0

    ℕ.next
    # => 1
    
    ℕ.next
    # => 2

    ℕ.rewind
    
    ℕ.next
    # => 0

[Enumerator.rewind]: http://ruby-doc.org/core-2.3.1/Enumerator.html#method-i-rewind

