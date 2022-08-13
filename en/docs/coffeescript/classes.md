---
title: "Classes"
slug: "classes"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Classes, Inheritance, and Super
CoffeeScript provides a basic class structure that allows you to name your class, set the superclass, assign prototypal properties, and define the constructor, in a single assignable expression.

Small example below:

    class Animal
      constructor: (@name) ->

      move: (meters) ->
        alert @name + " moved #{meters}m."

    class Snake extends Animal
      move: ->
        alert "Slithering..."
        super 5

    class Horse extends Animal
      move: ->
        alert "Galloping..."
        super 45

    sam = new Snake "Sammy the Python"
    tom = new Horse "Tommy the Palomino"

    sam.move()
    tom.move()

This will show 4 popups:

1. Slithering...
2. Sammy the Python moved 5m.
3. Galloping...
4. Tommy the Palomino moved 45m.

## Prototypes
If you feel the need to extend an object's prototype, `::` gives you quick access to an it so you can add methods to it and later use this method on all instances of that method.

    String::dasherize = ->
      this.replace /_/g, "-"

The above example will give you the ability to use the dasherize method on all Strings. This will replace all underscores to dashes.

