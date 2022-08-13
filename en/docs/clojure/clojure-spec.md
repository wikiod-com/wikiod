---
title: "clojure.spec"
slug: "clojurespec"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

## Syntax
- :: is a shorthand a namespace-qualified keyword. E.g. if we are in the namespace user: ::foo is a shorthand for :user/foo
- #: or # - map-literal syntax for qualifying keys in a map by a namespace

Clojure [spec][1] is a new specification/contracts library for clojure available as of version 1.9.

Specs are leveraged in a number of ways including being included in documentation, data validation, generating data for testing and more.

  [1]: http://clojure.org/guides/spec

## Using a predicate as a spec
Any predicate function can be used as a spec. Here's a simple example:

    (clojure.spec/valid? odd? 1)
    ;;=> true

    (clojure.spec/valid? odd? 2)
    ;;=> false

the `valid?` function will take a spec and a value and return true if the value conforms to the spec and false otherwise.

One other interesting predicate is set membership:

    (s/valid? #{:red :green :blue} :red) 
    ;;=> true


## fdef: writing a spec for a function
Let's say we have the following function:

    (defn nat-num-count [nums] (count (remove neg? nums)))

We can write a spec for this function by defining a function spec of the same name:

    (clojure.spec/fdef nat-num-count
            :args (s/cat :nums (s/coll-of number?))
            :ret integer?
            :fn #(<= (:ret %) (-> % :args :nums count)))

`:args` takes a regex spec which describes the sequence of arguments by a keyword label corresponding to the argument name and a corresponding spec. The reason the spec required by `:args` is a regex spec is to support multiple arities for a function. `:ret` specifies a spec for the return value of the function.

`:fn` is a spec which constrains the relationship between the `:args` and the `:ret`. It is used as a property when run through test.check.
It is called with a single argument: a map with two keys: `:args` (the conformed arguments to the function) and `:ret` (the function's conformed return value).

## Registering a spec
In addition to predicates functioning as specs, you can register a spec globally using `clojure.spec/def`. `def` requires that a spec being registered is named by a namespace-qualified keyword:

    (clojure.spec/def ::odd-nums odd?)
    ;;=> :user/odd-nums
    
    (clojure.spec/valid? ::odd-nums 1)
    ;;=> true
    (clojure.spec/valid? ::odd-nums 2)
    ;;=> false

Once registered, a spec can be referenced globally anywhere in a Clojure program.

The `::odd-nums` syntax is a shorthand for `:user/odd-nums`, assuming we are in the `user` namespace. `::` will qualify the symbol it precedes with the current namesapce.

Rather than pass in the predicate, we can pass in the spec name to `valid?`, and it will work the same way.

## clojure.spec/and & clojure.spec/or
`clojure.spec/and` & `clojure.spec/or` can be used to create more complex specs, using multiple specs or predicates:

    (clojure.spec/def ::pos-odd (clojure.spec/and odd? pos?))
    
    (clojure.spec/valid? ::pos-odd 1)
    ;;=> true
    
    (clojure.spec/valid? ::pos-odd -3)
    ;;=> false

`or` works similarly, with one significant difference. When defining an `or` spec, you must tag each possible branch with a keyword. This is used in order to provide specific branches which fail in error messages:

    (clojure.spec/def ::big-or-small (clojure.spec/or :small #(< % 10) :big #(> % 100)))
    
    (clojure.spec/valid? ::big-or-small 1)
    ;;=> true

    (clojure.spec/valid? ::big-or-small 150)
    ;;=> true
    
    (clojure.spec/valid? ::big-or-small 20)
    ;;=> false

When conforming a spec using `or`, the applicable spec will be returned which made the value conform:

    (clojure.spec/conform ::big-or-small 5)
    ;; => [:small 5]

## Record specs
You can spec a record as follows:

    (clojure.spec/def ::name string?)
    (clojure.spec/def ::age pos-int?)
    (clojure.spec/def ::occupation string?)
    
    (defrecord Person [name age occupation])
    
    (clojure.spec/def ::person (clojure.spec/keys :req-un [::name ::age ::occupation]))

    (clojure.spec/valid? ::person (->Person "john doe" 25 "programmer"))
    ;;=> true

    (clojure.spec/valid? ::person (->Person "john doe" "25" "programmer"))
    ;;=> false

At some point in the future, a reader syntax or built-in support for qualifying record keys by the records' namespace may be introduced. This support already exists for maps.


## Map specs
You can spec a map by specifying which keys should be present in the map:

    (clojure.spec/def ::name string?)
    (clojure.spec/def ::age pos-int?)
    (clojure.spec/def ::occupation string?)
    
    (clojure.spec/def ::person (clojure.spec/keys :req [::name ::age ::occupation]))

    (clojure.spec/valid? ::person {::name "john" ::age 25 ::occupation "programmer"})
    ;; => true

`:req` is a vector of keys required to be present in the map. You can specify additional options such as `:opt`, a vector of keys which are optional.

The examples so far require that the keys in the name are namespace-qualified. But it's common for map keys to be unqualified. For this case, `clojure.spec` provides :req and :opt equivalents for unqualified keys: `:req-un` and `:opt-un`. Here's the same example, with unqualified keys:

    (clojure.spec/def ::name string?)
    (clojure.spec/def ::age pos-int?)
    (clojure.spec/def ::occupation string?)
    
    (clojure.spec/def ::person (clojure.spec/keys :req-un [::name ::age ::occupation]))

    (clojure.spec/valid? ::person {:name "john" :age 25 :occupation "programmer"})
    ;; => true

Notice how the specs provided in the `:req-un` vector as still qualified. clojure.spec, will automatically confirm the unqualified versions in the map when conforming the values.

namespace map literal syntax allows you to qualify all the keys of a map by a single namespace succinctly. For example:

    (clojure.spec/def ::name string?)
    (clojure.spec/def ::age pos-int?)
    (clojure.spec/def ::occupation string?)

    (clojure.spec/def ::person (clojure.spec/keys :req [::name ::age ::occupation]))

    (clojure.spec/valid? ::person #:user{:name "john" :age 25 :occupation "programmer"})
    ;;=> true

Notice the special `#:` reader syntax. We follow this with the namespace we wish to qualify all the map keys by. These will then be checked against the specs corresponding to the provided namespace.

## Collections
You can spec collections in a number of ways. coll-of allows you to spec collections and provide some additional constraints. Here's a simple example:

    (clojure.spec/valid? (clojure.spec/coll-of int?) [1 2 3])
    ;; => true

    (clojure.spec/valid? (clojure.spec/coll-of int?) '(1 2 3))
    ;; => true

Constraint options follow the main spec/predicate for the collection. You can constrain the collection type with `:kind` like this:

    (clojure.spec/valid? (clojure.spec/coll-of int? :kind vector?) [1 2 3])
    ;; => true

    (clojure.spec/valid? (clojure.spec/coll-of int? :kind vector?) '(1 2 3))
    ;; => false

The above is false because the collection passed in is not a vector.

    (clojure.spec/valid? (clojure.spec/coll-of int? :kind list?) '(1 2 3))
    ;; => true

    (clojure.spec/valid? (clojure.spec/coll-of int? :kind set?) #{1 2 3})
    ;; => true

    (clojure.spec/valid? (clojure.spec/coll-of int? :kind set?) #{1 "2" 3})
    ;; => false

The above is false because not all elements in the set are ints.

You can also constrain the size of the collection in a few ways:

    (clojure.spec/valid? (clojure.spec/coll-of int? :kind vector? :count 3) [1 2 3])
    ;; => true

        (clojure.spec/valid? (clojure.spec/coll-of int? :kind vector? :count 3) [1 2])
    ;; => false

    (clojure.spec/valid? (clojure.spec/coll-of int? :min-count 3 :max-count 5) [1 2 3])
    ;; => true

        (clojure.spec/valid? (clojure.spec/coll-of int? :min-count 3 :max-count 5) [1 2])
    ;; => false

You can also enforce uniqueness of the elements in the collection with `:distinct`:

    (clojure.spec/valid? (clojure.spec/coll-of int? :distinct true) [1 2])
    ;; => true

    (clojure.spec/valid? (clojure.spec/coll-of int? :distinct true) [2 2])
    ;; => false

`coll-of` ensures all elements in a sequence are checked. For large collections, this can be very inefficient. `every` behaves just like `coll-of`, except it only samples a relatively small number of the sequences' elements from for conformance. This works well for large collections. Here's an example:

    (clojure.spec/valid? (clojure.spec/every int? :distinct true) [1 2 3 4 5])
    ;; => true

`map-of` is similar to `coll-of`, but for maps. Since maps have both keys and values, you must supply both a spec for the key and a spec for the value:

    (clojure.spec/valid? (clojure.spec/map-of keyword? string?) {:red "red" :green "green"})
    ;; => true

Like `coll-of`, `map-of` checks conformance of all map key/values. For large maps this will be inefficient. Like `coll-of`, `map-of` supplies `every-kv` for efficiently sampling a relatively small number of values from a large map:

    (clojure.spec/valid? (clojure.spec/every-kv keyword? string?) {:red "red" :green "green"})
    ;; => true




 

## Sequences
spec can describe and be used with arbitrary sequences. It supports this via a number of regex spec operations.

    (clojure.spec/valid? (clojure.spec/cat :text string? :int int?) ["test" 1])
    ;;=> true

`cat` requires labels for each spec used to describe the sequence. cat describes a sequence of elements and a spec for each one.

`alt` is used to choose among a number of possible specs for a given element in a sequence. For example:

    (clojure.spec/valid? (clojure.spec/cat :text-or-int (clojure.spec/alt :text string? :int int?)) ["test"])
    ;;=> true

`alt` also requires that each spec is labeled by a keyword.

Regex sequences can be composed in some very interesting and powerful ways to create arbitrarily complex sequence-describing specs. Here's a slightly more complex example:

    (clojure.spec/def ::complex-seq (clojure.spec/+ (clojure.spec/cat :num int? :foo-map (clojure.spec/map-of keyword? int?))))
    (clojure.spec/valid? ::complex-seq [0 {:foo 3 :baz 1} 4 {:foo 4}])
    ;;=> true

Here `::complex-seq` will validate a sequence of one or more pairs of elements, the first being an int and the second being a map of keyword to int.

