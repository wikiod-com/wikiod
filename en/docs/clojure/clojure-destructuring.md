---
title: "Clojure destructuring"
slug: "clojure-destructuring"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

## Destructuring a map
Here's how you can destructure a map:

    (def my-map {:a 1 :b 2 :c 3})

Then, for example, within a let block you can extract values from the map very succinctly as follows:

    (let [{x :a y :c} my-map]
      (println ":a val:" x ", :c val: " y))
    ;; :a val: 1 , :c val: 3

Notice that the values being extracted in each mapping are on the left and the keys they are associated with are on the right.

If you want to destructure values to bindings with the same names as the keys you can use this shortcut:

    (let [{:keys [a c]} my-map]
      (println ":a val:" a ", :c val: " c))
    ;; :a val: 1 , :c val: 3

If your keys are strings you can use almost the same structure:

    (let [{:strs [foo bar]} {"foo" 1 "bar" 2}]
      (println "FOO:" foo  "BAR: " bar ))
    ;; FOO: 1 BAR: 2

And similarly for symbols:

    (let [{:syms [foo bar]} {'foo 1 'bar 2}]
      (println "FOO:" foo "BAR:" bar))
    ;; FOO: 1 BAR: 2

If you want to destructure a nested map, you can nest binding-forms explained above:

    (def data
      {:foo {:a 1
             :b 2}
       :bar {:a 10
             :b 20}})
    
    (let [{{:keys [a b]} :foo
           {a2 :a b2 :b} :bar} data]
      [a b a2 b2])
    ;; => [1 2 10 20]


## Overview
[Destructuring][1] allows you to extract data from various objects into distinct variables. In each example below, each variable is assigned to its own string (`a`=`"a"`, b=`"b"`, &c.)

| Type                  | Example                      | Value of `data` / *comment*    |
| --------------------- | ---------------------------------- | -------------------------|
| [**`vec`**][2]        | `(let [[a b c] data ...)`          | `["a" "b" "c"]`
| [nested **`vec`**][3] | `(let [[[a b] [c d]] data ...)`    | `[["a" "b"] ["c" "d"]]`
| [**`map`**][4]        | `(let [{a :a b :b c :c} data ...)` | `{:a "a" :b "b" :c "c"}`
| — alternative:        | `(let [{:keys [a b c]} data ...)`  | *When variables are named after the keys.*


## Tips:

 - [Default values can be provided using `:or`][5], otherwise the default is `nil`
 - [Use `& rest` to store a `seq` of any extra values in `rest`][6], otherwise the extra values are ignored
 - [A common and useful use of destructuring is for function parameters][7]
 - You can assign unwanted parts to a throw-away variable (conventionally: `_`)


  [1]: http://clojure.org/guides/destructuring
  [2]: https://www.wikiod.com/clojure/clojure-destructuring#Destructuring a vector
  [3]: https://www.wikiod.com/clojure/clojure-destructuring#Destructuring nested vectors
  [4]: https://www.wikiod.com/clojure/clojure-destructuring
  [5]: https://www.wikiod.com/clojure/clojure-destructuring#Destructuring a map with default values
  [6]: https://www.wikiod.com/clojure/clojure-destructuring#Destructuring remaining elements into a sequence
  [7]: https://www.wikiod.com/clojure/clojure-destructuring#Destructuring params of a fn

## Destructuring a vector
Here's how you can destructure a vector:

    (def my-vec [1 2 3])

Then, for example within a `let` block, you can extract values from the vector very succinctly as follows:

    (let [[x y] my-vec]
     (println "first element:" x ", second element: " y))
    ;; first element: 1 , second element: 2



## Destructuring nested vectors
You can destructure nested vectors:

    (def my-vec [[1 2] [3 4]])

    (let [[[a b][c d]] my-vec]
      (println a b c d))
    ;; 1 2 3 4



## Destructuring remaining elements into a sequence
Let's say you have a vector like so:

    (def my-vec [1 2 3 4 5 6])

And you want to extract the first 3 elements and get the remaining elements as a sequence. This can be done as follows:

    (let [[x y z & remaining] my-vec]
     (println "first:" x ", second:" y "third:" z "rest:" remaining))
    ;= first: 1 , second: 2 third: 3 rest: (4 5 6)



## Destructuring a map with default values
Sometimes you want to destructure key under a map which might not be present in the map, but you want a default value for the destructured value. You can do that this way:

```clojure
(def my-map {:a 3 :b 4})
(let [{a :a
       b :b
       :keys [c d]
       :or {a 1
            c 2}} my-map]
  (println a b c d))
  ;= 3 4 2 nil
```

## Destructuring params of a fn
Destructurling works in many places, as well as in the param list of an fn:

```clojure
(defn my-func [[_ a b]]
  (+ a b))

(my-func [1 2 3]) ;= 5
(my-func (range 5)) ;= 3
```

Destructuring also works for the `& rest` construct in the param list:

```clojure
(defn my-func2 [& [_ a b]]
  (+ a b))

(my-func2 1 2 3) ;= 5
(apply my-func2 (range 5)) ;= 3
```

## Converting the rest of a sequence to a map
Destructuring also gives you the ability to interpret a sequence as a map:

    (def my-vec [:a 1 :b 2])
    (def my-lst '("smthg else" :c 3 :d 4))
    
    (let [[& {:keys [a b]}] my-vec
          [s & {:keys [c d]} my-lst]
      (+ a b c d)) ;= 10
    
It is useful for defining functions with **named parameters**:

    (defn my-func [a b & {:keys [c d] :or {c 3 d 4}}]
      (println a b c d))
    
    (my-func 1 2) ;= 1 2 3 4
    (my-func 3 4 :c 5 :d 6) ;= 3 4 5 6



## Destructuring and giving a name to the original argument value
    (defn print-some-items 
       [[a b :as xs]]
      (println a)
      (println b)
      (println xs))

    (print-some-items [2 3])

This example prints the output

    2
    3
    [2 3]

The argument is destructured and the items `2` and `3` are assigned to the symbols `a` and `b`. The original argument, the entire vector `[2 3]`, is also assigned to the symbol `xs`. 


## Destructuring and binding to keys' name
Sometimes when destructuring maps, you would like to bind the destructured values to their respective key name. Depending on the granularity of the data structure, using the *standard* destructuring scheme may be a little bit *verbose*.

Let's say, we have a map based record like so :

    (def john {:lastname "McCarthy" :firstname "John" :country "USA"})

we would normally destructure it like so :

    (let [{lastname :lastname firstname :firstname country :country} john]
        (str firstname " " lastname ", " country))
    ;;"John McCarthy, USA"

here, the data structure is quite simple with only 3 slots (*firstname, lastname, country*) but imagine how cumbersome it would be if we had to repeat all the key names twice for more granular data structure (having way more slots than just 3).

Instead, a better way of handling this is by using `:keys` (since our keys are *keywords* here) and selecting the key name we would like to bind to like so :

    (let [{:keys [firstname lastname country]} john]
        (str firstname " " lastname ", " country))
    ;;"John McCarthy, USA"

The same *intuitive logic* applies for other key types like *symbols* (using `:syms` ) and plain old *strings* (using `:strs`)

    ;; using strings as keys
    (def john {"lastname" "McCarthy" "firstname" "John" "country" "USA"})
    ;;#'user/john

    ;; destructuring string-keyed map
    (let [{:strs [lastname firstname country]} john]
        (str firstname " " lastname ", " country))
    ;;"John McCarthy, USA"

    ;; using symbols as keys
    (def john {'lastname "McCarthy" 'firstname "John" 'country "USA"})
    
    ;; destructuring symbol-keyed map
    (let [{:syms [lastname firstname country]} john]
        (str firstname " " lastname ", " country))
    ;;"John McCarthy, USA"

