---
title: "Getting Started with Reagent"
slug: "getting-started-with-reagent"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Reagent is a library that implements elements of React.js into ClojureScript, like the creations of custom "tags", which is implemented in Reagent through functions.

## UI using Reagent and Hiccup
Reagent is an interface between ClojureScript and react. It allows you to define efficient React components using nothing but plain ClojureScript functions and data, that describe your UI using a Hiccup-like syntax.

Example:-

    (defn sample-component []
      [:div
       [:p "I am a component!"]
       [:p.someclass
        "I have " [:strong "bold"] "text."]])

Output:

I am a component!

I have **bold** text.



## Reagent Atoms
Reagent atoms are essentially the same as regular atoms in both Clojure and ClojureScript - they are essentially variables that can be altered. This is especially useful, as Clojure(Script)'s data types are mostly immutable - which means that to change the value of a variable, the variable has to be redeclared.

Normal atoms are incompatible with Reagent, and so Reagent has its own. They are declared like normal variables, except with an additional function wrapped around the value:

    (:require [reagent.core :as r])

    (def num (r/atom 1))

You can get the value of an atom in two ways:

    (deref num) ; => 1
    @num        ; => 1

To change the value of an atom, there are two commands, `swap!` and `reset!`.

- `swap!` is given commands, changes the atom's value based on the original value of the atom itself
- `reset!` is given a value, and changes the atom's value to the value given, regardless of what the atom was originally


    (swap! num inc) ; => (inc num) => num = 2
    (reset! num 5)  ; => num = 5

