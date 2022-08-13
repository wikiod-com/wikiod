---
title: "Transducers"
slug: "transducers"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Transducers are composable components for processing data independently of the context. So they can be used to process collections, streams, channels, etc. without knowledge of their input sources or output sinks.

The Clojure core library was extended in 1.7 so that the sequence functions like map, filter, take, etc. return a transducer when called without a sequence. Because transducers are functions with specific contracts, they can be composed using the normal `comp` function.

Transducers allow the lazyness to be controlled as they are consumed. For example `into` is eager as would be expected, but `sequence` will lazily consume the sequence through the transducer. However, the lazyness guarantee is different. Enough of the source will be consumed to produce an element initially:

    (take 0 (sequence (map #(do (prn '-> %) %)) (range 5)))
    ;; -> 0
    ;; => ()

Or decide if the list is empty:

    (take 0 (sequence (comp (map #(do (prn '-> %) %)) (remove number?)) (range 5)))
    ;; -> 0
    ;; -> 1
    ;; -> 2
    ;; -> 3
    ;; -> 4
    ;; => ()

Which differs from the usual lazy sequence behaviour:

    (take 0 (map #(do (prn '-> %) %) (range 5)))
    ;; => ()

## Small transducer applied to a vector
    (let [xf (comp
               (map inc)
               (filter even?))]
      (transduce xf + [1 2 3 4 5 6 7 8 9 10]))
    ;; => 30

This example creates a transducer assigned to the local `xf` and uses `transduce` to  apply it to some data. The transducer add's one to each of it's inputs and only returns the even numbers.

`transduce` is like `reduce`, and collapses the input collection to a single value using the provided `+` function.

This reads like the thread-last macro, but separates the input data from the computations.

    (->> [1 2 3 4 5 6 7 8 9 10]
         (map inc)
         (filter even?)
         (reduce +))
    ;; => 30

## Applying transducers
    (def xf (filter keyword?))

Apply to a collection, returning a sequence:

    (sequence xf [:a 1 2 :b :c]) ;; => (:a :b :c)

Apply to a collection, reducing the resulting collection with another function:

    (transduce xf str [:a 1 2 :b :c]) ;; => ":a:b:c"

Apply to a collection, and `conj` the result into another collection:

    (into [] xf [:a 1 2 :b :c]) ;; => [:a :b :c]

Create a core async channel that uses a transducer to filter messages:

    (require '[clojure.core.async :refer [chan >!! <!! poll!]])
    (doseq [e [:a 1 2 :b :c]] (>!! ch e))
    (<!! ch) ;; => :a
    (<!! ch) ;; => :b
    (<!! ch) ;; => :c
    (poll! ch) ;;=> nil


## Creating/Using Transducers
So the most used functions on Clojure map and filter have been modified to return transducers (composable algorithmic transformations), if not called with a collection. That means:

`(map inc)` returns a transducer and so does `(filter odd?)`

The advantage: the functions can be composed into a single function by comp , which means traversing the collection just once. Saves run time by over 50% in some scenarios.

Definition:

    (def composed-fn (comp (map inc) (filter odd?)))
Usage:

    ;; So instead of doing this:
    (->> [1 8 3 10 5]
         (map inc)
        (filter odd?))
    ;; Output [9 11]

    ;; We do this: 
    (into [] composed-fn [1 8 3 10 5])
    ;; Output: [9 11]


