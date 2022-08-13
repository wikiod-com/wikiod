---
title: "Collections and Sequences"
slug: "collections-and-sequences"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

## Syntax
 * `'()` → `()`
 * `'(1 2 3 4 5)` → `(1 2 3 4 5)`
 * `'(1 foo 2 bar 3)` → `(1 'foo 2 'bar 3)`
 * `(`[`list`][1] `1 2 3 4 5)` → `(1 2 3 4 5)`
 * `(`[`list*`][2] `[1 2 3 4 5])` → `(1 2 3 4 5)`
 * `[]` → `[]`
 * `[1 2 3 4 5]` → `[1 2 3 4 5]`
 * `(`[`vector`][3] `1 2 3 4 5)` → `[1 2 3 4 5]`
 * `(`[`vec`][4] `'(1 2 3 4 5))` → `[1 2 3 4 5]`
 * `{}` => `{}`
 * `{:keyA 1 :keyB 2}` → `{:keyA 1 :keyB 2}`
 * `{:keyA 1, :keyB 2}` → `{:keyA 1 :keyB 2}`
 * `(`[`hash-map`][5] `:keyA 1 :keyB 2)` → `{:keyA 1 :keyB 2}`
 * `(`[`sorted-map`][6] `5 "five" 1 "one")` → `{1 "one" 5 "five"}` (entries are sorted by key when used as a sequence)
 * `#{}` → `#{}`
 * `#{1 2 3 4 5}` → `#{4 3 2 5 1}` (unordered)
 * `(`[`hash-set`][7] `1 2 3 4 5)` → `#{2 5 4 1 3}` (unordered)
 * `(`[`sorted-set`][8] `2 5 4 3 1)` → `#{1 2 3 4 5}`

  [1]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/list
  [2]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/list*
  [3]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/vector
  [4]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/vec
  [5]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/hash-map
  [6]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/sorted-map
  [7]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/hash-set
  [8]: http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/sorted-set

## Lists
A list is denoted by parentheses:

    ()
    ;;=> ()

A Clojure list is a [singly linked list]. `conj` "conjoins" a new element to the collection in the most efficient location. For lists, this is at the beginning:

    (conj () :foo)
    ;;=> (:foo)

    (conj (conj () :bar) :foo)
    ;;=> (:foo :bar)

Unlike other collections, non-empty lists are evaluated as calls to special forms, macros, or functions when evaluated. Therefore, while `(:foo)` *is* the literal representation of the list containing `:foo` as its only item, evaluating `(:foo)` in a REPL will cause an [`IllegalArgumentException`][illegalargumentexception] to be thrown because a keyword cannot be invoked as a [nullary function].

    (:foo)
    ;; java.lang.IllegalArgumentException: Wrong number of args passed to keyword: :foo

To prevent Clojure from evaluating a non-empty list, you can [`quote`][quote] it:

    '(:foo)
    ;;=> (:foo)

    '(:foo :bar)
    ;;=> (:foo :bar)

Unfortunately, this causes the elements to not be evaluated:

    (+ 1 1)
    ;;=> 2

    '(1 (+ 1 1) 3)
    ;;=> (1 (+ 1 1) 3)

For this reason, you'll usually want to use [`list`][list], a [variadic function] that evaluates all of its arguments and uses those results to construct a list:

    (list)
    ;;=> ()

    (list :foo)
    ;;=> (:foo)

    (list :foo :bar)
    ;;=> (:foo :bar)

    (list 1 (+ 1 1) 3)
    ;;=> (1 2 3)

`count` returns the number of items, in constant time:

    (count ())
    ;;=> 0

    (count (conj () :foo))
    ;;=> 1

    (count '(:foo :bar))
    ;;=> 2

You can test whether something is a list using the [`list?`][list?] predicate:

    (list? ())
    ;;=> true

    (list? '(:foo :bar))
    ;;=> true

    (list? nil)
    ;;=> false

    (list? 42)
    ;;=> false

    (list? :foo)
    ;;=> false

You can get the first element of a list using [`peek`][peek]:

    (peek ())
    ;;=> nil

    (peek '(:foo))
    ;;=> :foo

    (peek '(:foo :bar))
    ;;=> :foo

You can get a new list without the first element using [`pop`][pop]:

    (pop '(:foo))
    ;;=> ()

    (pop '(:foo :bar))
    ;;=> (:bar)

Note that if you try to `pop` an empty list, you'll get an [`IllegalStateException`][illegalstateexception]:

    (pop ())
    ;; java.lang.IllegalStateException: Can't pop empty list

Finally, all lists are sequences, so you can do everything with a list that you can do with any other sequence. Indeed, with the exception of the empty list, calling `seq` on a list returns the exact same object:

    (seq ())
    ;;=> nil

    (seq '(:foo))
    ;;=> (:foo)

    (seq '(:foo :bar))
    ;;=> (:foo :bar)

    (let [x '(:foo :bar)]
      (identical? x (seq x)))
    ;;=> true

[illegalargumentexception]: https://docs.oracle.com/javase/8/docs/api/java/lang/IllegalArgumentException.html
[illegalstateexception]: https://docs.oracle.com/javase/8/docs/api/java/lang/IllegalStateException.html
[list]: https://clojuredocs.org/clojure.core/list
[list?]: https://clojuredocs.org/clojure.core/list_q
[nullary function]: https://en.wikipedia.org/wiki/Arity#Nullary
[peek]: https://clojuredocs.org/clojure.core/peek
[pop]: https://clojuredocs.org/clojure.core/pop
[quote]: http://clojuredocs.org/clojure.core/quote
[singly linked list]: https://en.wikipedia.org/wiki/Linked_list#Singly_linked_list
[variadic function]: https://en.wikipedia.org/wiki/Variadic_function

## Maps
Unlike the list, which is a sequential data structure, and the vector, which is both sequential and associative, the map is exclusively an associative data structure. A map consists of a set of mappings from keys to values. All keys are unique, so maps support "constant"-time lookup from keys to values.

A map is denoted by curly braces:

    {}
    ;;=> {}

    {:foo :bar}
    ;;=> {:foo :bar}

    {:foo :bar :baz :qux}
    ;;=> {:foo :bar, :baz :qux}

Each pair of two elements is a key-value pair. So, for instance, the first map above has no mappings. The second has one mapping, from the key `:foo` to the value `:bar`. The third has two mappings, one from the key `:foo` to the value `:bar`, and one from the key `:baz` to the value `:qux`. Maps are inherently unordered, so the order in which the mappings appear doesn't matter:

    (= {:foo :bar :baz :qux}
       {:baz :qux :foo :bar})
    ;;=> true

You can test whether something is a map using the [`map?`][map?] predicate:

    (map? {})
    ;;=> true

    (map? {:foo :bar})
    ;;=> true

    (map? {:foo :bar :baz :qux})
    ;;=> true

    (map? nil)
    ;;=> false

    (map? 42)
    ;;=> false

    (map? :foo)
    ;;=> false

You can test whether a map contains a given *key* in "constant" time using the [`contains?`][contains?] predicate:

    (contains? {:foo :bar :baz :qux} 42)
    ;;=> false

    (contains? {:foo :bar :baz :qux} :foo)
    ;;=> true

    (contains? {:foo :bar :baz :qux} :bar)
    ;;=> false

    (contains? {:foo :bar :baz :qux} :baz)
    ;;=> true

    (contains? {:foo :bar :baz :qux} :qux)
    ;;=> false

    (contains? {:foo nil} :foo)
    ;;=> true

    (contains? {:foo nil} :bar)
    ;;=> false

You can get the value associated with a key using [`get`][get]:

    (get {:foo :bar :baz :qux} 42)
    ;;=> nil

    (get {:foo :bar :baz :qux} :foo)
    ;;=> :bar

    (get {:foo :bar :baz :qux} :bar)
    ;;=> nil

    (get {:foo :bar :baz :qux} :baz)
    ;;=> :qux

    (get {:foo :bar :baz :qux} :qux)
    ;;=> nil

    (get {:foo nil} :foo)
    ;;=> nil

    (get {:foo nil} :bar)
    ;;=> nil

In addition, maps themselves are functions that take a key and return the value associated with that key:

    ({:foo :bar :baz :qux} 42)
    ;;=> nil

    ({:foo :bar :baz :qux} :foo)
    ;;=> :bar

    ({:foo :bar :baz :qux} :bar)
    ;;=> nil

    ({:foo :bar :baz :qux} :baz)
    ;;=> :qux

    ({:foo :bar :baz :qux} :qux)
    ;;=> nil

    ({:foo nil} :foo)
    ;;=> nil

    ({:foo nil} :bar)
    ;;=> nil

You can get an entire map entry (key and value together) as a two-element vector using [`find`][find]:

    (find {:foo :bar :baz :qux} 42)
    ;;=> nil

    (find {:foo :bar :baz :qux} :foo)
    ;;=> [:foo :bar]

    (find {:foo :bar :baz :qux} :bar)
    ;;=> nil

    (find {:foo :bar :baz :qux} :baz)
    ;;=> [:baz :qux]

    (find {:foo :bar :baz :qux} :qux)
    ;;=> nil

    (find {:foo nil} :foo)
    ;;=> [:foo nil]

    (find {:foo nil} :bar)
    ;;=> nil

You can extract the key or value from a map entry using [`key`][key] or [`val`][val], respectively:

    (key (find {:foo :bar} :foo))
    ;;=> :foo

    (val (find {:foo :bar} :foo))
    ;;=> :bar
Note that, although all Clojure map entries are vectors, not all vectors are map entries. If you try to call `key` or `val` on anything that's not a map entry, you'll get a [`ClassCastException`][classcastexception]:

    (key [:foo :bar])
    ;; java.lang.ClassCastException:

    (val [:foo :bar])
    ;; java.lang.ClassCastException:

You can test whether something is a map entry using the [`map-entry?`][map-entry?] predicate:

    (map-entry? (find {:foo :bar} :foo))
    ;;=> true

    (map-entry? [:foo :bar])
    ;;=> false

You can use [`assoc`][assoc] to get a map that has all the same key-value pairs as an existing map, with one mapping added or changed:

    (assoc {} :foo :bar)
    ;;=> {:foo :bar}

    (assoc (assoc {} :foo :bar) :baz :qux)
    ;;=> {:foo :bar, :baz :qux}

    (assoc {:baz :qux} :foo :bar)
    ;;=> {:baz :qux, :foo :bar}

    (assoc {:foo :bar :baz :qux} :foo 42)
    ;;=> {:foo 42, :baz :qux}

    (assoc {:foo :bar :baz :qux} :baz 42)
    ;;=> {:foo :bar, :baz 42}

You can use [`dissoc`][dissoc] to get a map that has all the same key-value pairs as an existing map, with possibly one mapping removed:

    (dissoc {:foo :bar :baz :qux} 42)
    ;;=> {:foo :bar :baz :qux}

    (dissoc {:foo :bar :baz :qux} :foo)
    ;;=> {:baz :qux}

    (dissoc {:foo :bar :baz :qux} :bar)
    ;;=> {:foo :bar :baz :qux}

    (dissoc {:foo :bar :baz :qux} :baz)
    ;;=> {:foo :bar}

    (dissoc {:foo :bar :baz :qux} :qux)
    ;;=> {:foo :bar :baz :qux}

    (dissoc {:foo nil} :foo)
    ;;=> {}

`count` returns the number of mappings, in constant time:

    (count {})
    ;;=> 0

    (count (assoc {} :foo :bar))
    ;;=> 1

    (count {:foo :bar :baz :qux})
    ;;=> 2

You can get a sequence of all entries in a map using `seq`:

    (seq {})
    ;;=> nil

    (seq {:foo :bar})
    ;;=> ([:foo :bar])

    (seq {:foo :bar :baz :qux})
    ;;=> ([:foo :bar] [:baz :qux])

Again, maps are unordered, so the ordering of the items in a sequence that you get by calling `seq` on a map is undefined.

You can get a sequence of just the keys or just the values in a map using [`keys`][keys] or [`vals`][vals], respectively:

    (keys {})
    ;;=> nil

    (keys {:foo :bar})
    ;;=> (:foo)

    (keys {:foo :bar :baz :qux})
    ;;=> (:foo :baz)

    (vals {})
    ;;=> nil

    (vals {:foo :bar})
    ;;=> (:bar)

    (vals {:foo :bar :baz :qux})
    ;;=> (:bar :qux)

Clojure 1.9 adds a literal syntax for more concisely representing a map where the keys share the same namespace. Note that the map in either case is identical (the map does not "know" the default namespace), this is merely a syntactic convenience.

```
;; typical map syntax
(def p {:person/first"Darth" :person/last "Vader" :person/email "darth@death.star"})

;; namespace map literal syntax
(def p #:person{:first "Darth" :last "Vader" :email "darth@death.star"})
```

[assoc]: https://clojuredocs.org/clojure.core/assoc
[classcastexception]: https://docs.oracle.com/javase/8/docs/api/java/lang/ClassCastException.html
[contains?]: https://clojuredocs.org/clojure.core/contains_q
[dissoc]: https://clojuredocs.org/clojure.core/dissoc
[find]: https://clojuredocs.org/clojure.core/find
[get]: https://clojuredocs.org/clojure.core/get
[key]: https://clojuredocs.org/clojure.core/key
[keys]: https://clojuredocs.org/clojure.core/key
[map-entry?]: https://clojuredocs.org/clojure.core/map-entry_q
[map?]: https://clojuredocs.org/clojure.core/map_q
[val]: https://clojuredocs.org/clojure.core/val
[vals]: https://clojuredocs.org/clojure.core/vals

## Collections
All built-in Clojure collections are immutable and heterogeneous, have literal syntax, and support the [`conj`][conj], [`count`][count], and [`seq`][seq] functions.

- `conj` returns a new collection that is equivalent to an existing collection with an item "added", in either "constant" or logarithmic time. What exactly this means depends on the collection. 
- `count` returns the number of items in a collection, in constant time.
- `seq` returns `nil` for an empty collection, or a sequence of items for a non-empty collection, in constant time.

[conj]: https://clojuredocs.org/clojure.core/conj
[count]: https://clojuredocs.org/clojure.core/count
[seq]: https://clojuredocs.org/clojure.core/seq

## Vectors
A vector is denoted by square brackets:

    []
    ;;=> []

    [:foo]
    ;;=> [:foo]

    [:foo :bar]
    ;;=> [:foo :bar]

    [1 (+ 1 1) 3]
    ;;=> [1 2 3]

In addition using to the literal syntax, you can also use the [`vector`][vector] function to construct a vector:

    (vector)
    ;;=> []

    (vector :foo)
    ;;=> [:foo]

    (vector :foo :bar)
    ;;=> [:foo :bar]

    (vector 1 (+ 1 1) 3)
    ;;=> [1 2 3]

You can test whether something is a vector using the [`vector?`][vector?] predicate:

    (vector? [])
    ;;=> true

    (vector? [:foo :bar])
    ;;=> true

    (vector? nil)
    ;;=> false

    (vector? 42)
    ;;=> false

    (vector? :foo)
    ;;=> false

`conj` adds elements to the end of a vector:

    (conj [] :foo)
    ;;=> [:foo]

    (conj (conj [] :foo) :bar)
    ;;=> [:foo :bar]
    
    (conj [] :foo :bar)
    ;;=> [:foo :bar]

`count` returns the number of items, in constant time:

    (count [])
    ;;=> 0

    (count (conj [] :foo))
    ;;=> 1

    (count [:foo :bar])
    ;;=> 2

You can get the last element of a vector using [`peek`][peek]:

    (peek [])
    ;;=> nil

    (peek [:foo])
    ;;=> :foo

    (peek [:foo :bar])
    ;;=> :bar

You can get a new vector without the last element using [`pop`][pop]:

    (pop [:foo])
    ;;=> []

    (pop [:foo :bar])
    ;;=> [:foo]

Note that if you try to pop an empty vector, you'll get an [`IllegalStateException`][illegalstateexception]:

    (pop [])
    ;; java.lang.IllegalStateException: Can't pop empty vector

Unlike lists, vectors are indexed. You can get an element of a vector by index in "constant" time using [`get`][get]:

    (get [:foo :bar] 0)
    ;;=> :foo

    (get [:foo :bar] 1)
    ;;=> :bar

    (get [:foo :bar] -1)
    ;;=> nil

    (get [:foo :bar] 2)
    ;;=> nil

In addition, vectors themselves are functions that take an index and return the element at that index:

    ([:foo :bar] 0)
    ;;=> :foo

    ([:foo :bar] 1)
    ;;=> :bar

However, if you call a vector with an invalid index, you'll get an [`IndexOutOfBoundsException`][indexoutofboundsexception] instead of `nil`:

    ([:foo :bar] -1)
    ;; java.lang.IndexOutOfBoundsException:

    ([:foo :bar] 2)
    ;; java.lang.IndexOutOfBoundsException:

You can get a new vector with a different value at a particular index using [`assoc`][assoc]:

    (assoc [:foo :bar] 0 42)
    ;;=> [42 :bar]

    (assoc [:foo :bar] 1 42)
    ;;=> [:foo 42]

If you pass an index equal to the `count` of the vector, Clojure will add the element as if you had used `conj`. However, if you pass an index that is negative or greater than the `count`, you'll get an [`IndexOutOfBoundsException`][indexoutofboundsexception]:

    (assoc [:foo :bar] 2 42)
    ;;=> [:foo :bar 42]

    (assoc [:foo :bar] -1 42)
    ;; java.lang.IndexOutOfBoundsException:

    (assoc [:foo :bar] 3 42)
    ;; java.lang.IndexOutOfBoundsException:

You can get a sequence of the items in a vector using `seq`:

    (seq [])
    ;;=> nil

    (seq [:foo])
    ;;=> (:foo)

    (seq [:foo :bar])
    ;;=> (:foo :bar)

Since vectors are indexed, you can also get a reversed sequence of a vector's items using [`rseq`][rseq]:

    (rseq [])
    ;;=> nil

    (rseq [:foo])
    ;;=> (:foo)

    (rseq [:foo :bar])
    ;;=> (:bar :foo)

Note that, although all lists are sequences, and sequences are displayed in the same way as lists, not all sequences are lists!

    '(:foo :bar)
    ;;=> (:foo :bar)

    (seq [:foo :bar])
    ;;=> (:foo :bar)

    (list? '(:foo :bar))
    ;;=> true

    (list? (seq [:foo :bar]))
    ;;=> false

    (list? (rseq [:foo :bar]))
    ;;=> false

[assoc]: https://clojuredocs.org/clojure.core/assoc
[get]: https://clojuredocs.org/clojure.core/get
[illegalstateexception]: https://docs.oracle.com/javase/8/docs/api/java/lang/IllegalStateException.html
[indexoutofboundsexception]: https://docs.oracle.com/javase/8/docs/api/java/lang/IndexOutOfBoundsException.html
[peek]: https://clojuredocs.org/clojure.core/peek
[pop]: https://clojuredocs.org/clojure.core/pop
[rseq]: https://clojuredocs.org/clojure.core/rseq
[vector]: https://clojuredocs.org/clojure.core/vector
[vector?]: https://clojuredocs.org/clojure.core/vector_q

## Sets
Like maps, sets are associative and unordered. Unlike maps, which contain mappings from keys to values, sets essentially map from keys to themselves.

A set is denoted by curly braces preceded by an octothorpe:

    #{}
    ;;=> #{}

    #{:foo}
    ;;=> #{:foo}

    #{:foo :bar}
    ;;=> #{:bar :foo}

As with maps, the order in which elements appear in a literal set doesn't matter:

    (= #{:foo :bar} #{:bar :foo})
    ;;=> true

You can test whether something is a set using the [`set?`][set?] predicate:

    (set? #{})
    ;;=> true

    (set? #{:foo})
    ;;=> true

    (set? #{:foo :bar})
    ;;=> true

    (set? nil)
    ;;=> false

    (set? 42)
    ;;=> false

    (set? :foo)
    ;;=> false

You can test whether a map contains a given item in "constant" time using the [`contains?`][contains?] predicate:

    (contains? #{} :foo)
    ;;=> false

    (contains? #{:foo} :foo)
    ;;=> true

    (contains? #{:foo} :bar)
    ;;=> false

    (contains? #{} nil)
    ;;=> false

    (contains? #{nil} nil)
    ;;=> true

In addition, sets themselves are functions that take an element and return that element if it is present in the set, or `nil` if it isn't:

    (#{} :foo)
    ;;=> nil

    (#{:foo} :foo)
    ;;=> :foo

    (#{:foo} :bar)
    ;;=> nil

    (#{} nil)
    ;;=> nil

    (#{nil} nil)
    ;;=> nil

You can use `conj` to get a set that has all the elements of an existing set, plus one additional item:

    (conj #{} :foo)
    ;;=> #{:foo}

    (conj (conj #{} :foo) :bar)
    ;;=> #{:bar :foo}

    (conj #{:foo} :foo)
    ;;=> #{:foo}

You can use [`disj`][disj] to get a set that has all the elements of an existing set, minus one item:

    (disj #{} :foo)
    ;;=> #{}

    (disj #{:foo} :foo)
    ;;=> #{}

    (disj #{:foo} :bar)
    ;;=> #{:foo}

    (disj #{:foo :bar} :foo)
    ;;=> #{:bar}

    (disj #{:foo :bar} :bar)
    ;;=> #{:foo}

`count` returns the number of elements, in constant time:

    (count #{})
    ;;=> 0

    (count (conj #{} :foo))
    ;;=> 1

    (count #{:foo :bar})
    ;;=> 2

You can get a sequence of all elements in a set using `seq`:

    (seq #{})
    ;;=> nil

    (seq #{:foo})
    ;;=> (:foo)

    (seq #{:foo :bar})
    ;;=> (:bar :foo)

[contains?]: https://clojuredocs.org/clojure.core/contains_q
[disj]: https://clojuredocs.org/clojure.core/disj
[set?]: https://clojuredocs.org/clojure.core/set_q

## Sequences
A sequence is very much like a list: it is an immutable object that can give you its [`first`][first] element or the [`rest`][rest] of its elements in constant time. You can also [`cons`][cons]truct a new sequence from an existing sequence and an item to stick at the beginning.

You can test whether something is a sequence using the [`seq?`][seq?] predicate:

    (seq? nil)
    ;;=> false

    (seq? 42)
    ;;=> false

    (seq? :foo)
    ;;=> false

As you already know, lists are sequences:

    (seq? ())
    ;;=> true

    (seq? '(:foo :bar))
    ;;=> true

Anything you get by calling [`seq`][seq] or [`rseq`][rseq] or [`keys`][keys] or [`vals`][vals] on a non-empty collection is also a sequence:

    (seq? (seq ()))
    ;;=> false

    (seq? (seq '(:foo :bar)))
    ;;=> true

    (seq? (seq []))
    ;;=> false

    (seq? (seq [:foo :bar]))
    ;;=> true

    (seq? (rseq []))
    ;;=> false

    (seq? (rseq [:foo :bar]))
    ;;=> true

    (seq? (seq {}))
    ;;=> false

    (seq? (seq {:foo :bar :baz :qux}))
    ;;=> true

    (seq? (keys {}))
    ;;=> false

    (seq? (keys {:foo :bar :baz :qux}))
    ;;=> true

    (seq? (vals {}))
    ;;=> false

    (seq? (vals {:foo :bar :baz :qux}))
    ;;=> true

    (seq? (seq #{}))
    ;;=> false

    (seq? (seq #{:foo :bar}))
    ;;=> true

Remember that all lists are sequences, but not all sequences are lists. While lists support [`peek`][peek] and [`pop`][pop] and [`count`][count] in constant time, in general, a sequence does not need to support any of those functions. If you try to call `peek` or `pop` on a sequence that doesn't also support Clojure's stack interface, you'll get a [`ClassCastException`][classcastexception]:

    (peek (seq [:foo :bar]))
    ;; java.lang.ClassCastException: clojure.lang.PersistentVector$ChunkedSeq cannot be cast to clojure.lang.IPersistentStack

    (pop (seq [:foo :bar]))
    ;; java.lang.ClassCastException: clojure.lang.PersistentVector$ChunkedSeq cannot be cast to clojure.lang.IPersistentStack

    (peek (seq #{:foo :bar}))
    ;; java.lang.ClassCastException: clojure.lang.APersistentMap$KeySeq cannot be cast to clojure.lang.IPersistentStack

    (pop (seq #{:foo :bar}))
    ;; java.lang.ClassCastException: clojure.lang.APersistentMap$KeySeq cannot be cast to clojure.lang.IPersistentStack

    (peek (seq {:foo :bar :baz :qux}))
    ;; java.lang.ClassCastException: clojure.lang.PersistentArrayMap$Seq cannot be cast to clojure.lang.IPersistentStack

    (pop (seq {:foo :bar :baz :qux}))
    ;; java.lang.ClassCastException: clojure.lang.PersistentArrayMap$Seq cannot be cast to clojure.lang.IPersistentStack

If you call `count` on a sequence that doesn't implement `count` in constant time, you won't get an error; instead, Clojure will traverse the entire sequence until it reaches the end, then return the number of elements that it traversed. This means that, for general sequences, `count` is linear, not constant, time. You can test whether something supports constant-time `count` using the [`counted?`][counted?] predicate:

    (counted? '(:foo :bar))
    ;;=> true

    (counted? (seq '(:foo :bar)))
    ;;=> true

    (counted? [:foo :bar])
    ;;=> true

    (counted? (seq [:foo :bar]))
    ;;=> true

    (counted? {:foo :bar :baz :qux})
    ;;=> true

    (counted? (seq {:foo :bar :baz :qux}))
    ;;=> true

    (counted? #{:foo :bar})
    ;;=> true

    (counted? (seq #{:foo :bar}))
    ;;=> false

As mentioned above, you can use `first` to get the first element of a sequence. Note that `first` will call `seq` on their argument, so it can be used on anything "seqable", not just actual sequences:

    (first nil)
    ;;=> nil

    (first '(:foo))
    ;;=> :foo

    (first '(:foo :bar))
    ;;=> :foo

    (first [:foo])
    ;;=> :foo

    (first [:foo :bar])
    ;;=> :foo

    (first {:foo :bar})
    ;;=> [:foo :bar]

    (first #{:foo})
    ;;=> :foo

Also as mentioned above, you can use `rest` to get a sequence containing all but the first element of an existing sequence. Like `first`, it calls  `seq` on its argument. However, it does *not* call `seq` on its result! This means that, if you call `rest` on a sequence that contains fewer than two items, you'll get back `()` instead of `nil`:

    (rest nil)
    ;;=> ()

    (rest '(:foo))
    ;;=> ()

    (rest '(:foo :bar))
    ;;=> (:bar)

    (rest [:foo])
    ;;=> ()

    (rest [:foo :bar])
    ;;=> (:bar)

    (rest {:foo :bar})
    ;;=> ()

    (rest #{:foo})
    ;;=> ()

If you want to get back `nil` when there aren't any more elements in a sequence, you can use [`next`][next] instead of `rest`:

    (next nil)
    ;;=> nil

    (next '(:foo))
    ;;=> nil

    (next [:foo])
    ;;=> nil

You can use the `cons` function to create a new sequence that will return its first argument for `first` and its second argument for `rest`:

    (cons :foo nil)
    ;;=> (:foo)

    (cons :foo (cons :bar nil))
    ;;=> (:foo :bar)

Clojure provides a large [sequence library] with many functions for dealing with sequences. The important thing about this library is that it works with anything "seqable", not just lists. That's why the concept of a sequence is so useful; it means that a single function, like [`reduce`][reduce], works perfectly on any collection:

    (reduce + '(1 2 3))
    ;;=> 6

    (reduce + [1 2 3])
    ;;=> 6

    (reduce + #{1 2 3})
    ;;=> 6

The other reason that sequences are useful is that, since they don't mandate any particular implementation of `first` and `rest`, they allow for lazy sequences whose elements are only realized when necessary.

Given an expression that would create a sequence, you can wrap that expression in the [`lazy-seq`][lazy-seq] macro to get an object that acts like a sequence, but will only actually evaluate that expression when it is asked to do so by the [`seq`][seq] function, at which point it will cache the result of the expression and forward `first` and `rest` calls to the cached result.

For finite sequences, a lazy sequence usually acts the same as an equivalent eager sequence:

    (seq [:foo :bar])
    ;;=> (:foo :bar)

    (lazy-seq [:foo :bar])
    ;;=> (:foo :bar)

However, the difference becomes apparent for infinite sequences:

    (defn eager-fibonacci [a b]
      (cons a (eager-fibonacci b (+' a b))))

    (defn lazy-fibonacci [a b]
      (lazy-seq (cons a (lazy-fibonacci b (+' a b)))))

    (take 10 (eager-fibonacci 0 1))
    ;; java.lang.StackOverflowError:

    (take 10 (lazy-fibonacci 0 1))
    ;;=> (0 1 1 2 3 5 8 13 21 34)

[classcastexception]: https://docs.oracle.com/javase/8/docs/api/java/lang/ClassCastException.html
[cons]: https://clojuredocs.org/clojure.core/cons
[count]: https://clojuredocs.org/clojure.core/count
[counted?]: https://clojuredocs.org/clojure.core/counted_q
[first]: https://clojuredocs.org/clojure.core/first
[keys]: https://clojuredocs.org/clojure.core/keys
[lazy-seq]: https://clojuredocs.org/clojure.core/lazy-seq
[next]: https://clojuredocs.org/clojure.core/next
[peek]: https://clojuredocs.org/clojure.core/peek
[pop]: https://clojuredocs.org/clojure.core/pop
[reduce]: https://clojuredocs.org/clojure.core/reduce
[rest]: https://clojuredocs.org/clojure.core/rest
[rseq]: https://clojuredocs.org/clojure.core/rseq
[seq]: https://clojuredocs.org/clojure.core/seq
[seq?]: https://clojuredocs.org/clojure.core/seq_q
[sequence library]: http://clojure.org/reference/sequences#_the_seq_library
[vals]: https://clojuredocs.org/clojure.core/vals

