---
title: "Atom"
slug: "atom"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

An atom in Clojure is a variable that can be changed throughout your program (namespace). Because most data types in Clojure are immutable (or unchangeable) - you can't change a number's value without redefining it - atoms are essential in Clojure programming.

## Define an atom
To define an atom, use an ordinary `def`, but add an `atom` function before it, like so:

    (def counter (atom 0))

This creates an `atom` of value `0`. Atoms can be of any type:

    (def foo (atom "Hello"))
    (def bar (atom ["W" "o" "r" "l" "d"]))

## Read an atom's value
To read an atom's value, simply put the name of the atom, with a `@` before it:

<!-- lang-all: lang-clojure -->

    @counter ; => 0

A bigger example:

    (def number (atom 3))
    (println (inc @number))
    ;; This should output 4

## Update an atom's value
There are two commands to change an atom, `swap!` and `reset!`. `swap!` is given commands, and changes the atom based on its current state. `reset!` changes the atom's value completely, regardless of what the original atom's value was:

<!-- lang-all: lang-clojure -->

    (swap! counter inc) ; => 1
    (reset! counter 0) ; => 0

This example outputs the first 10 powers of `2` using atoms:

    (def count (atom 0))

    (while (< @atom 10)
      (swap! atom inc)
      (println (Math/pow 2 @atom)))

