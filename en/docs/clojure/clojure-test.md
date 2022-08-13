---
title: "clojure.test"
slug: "clojuretest"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## is
The [`is`][is] macro is the core of the `clojure.test` library. It returns the value of its body expression, printing an error message if the expression returns a falsey value.

    (defn square [x]
      (+ x x))

    (require '[clojure.test :as t])

    (t/is (= 0 (square 0)))
    ;;=> true

    (t/is (= 1 (square 1)))
    ;;
    ;; FAIL in () (foo.clj:1)
    ;; expected: (= 1 (square 1))
    ;;   actual: (not (= 1 2))
    ;;=> false

[is]: https://clojuredocs.org/clojure.test/is

## Wrap each test or all tests with use-fixtures
`use-fixtures` allows to wrap each `deftest` in namespace with code that runs before and after test. It can be used for fixtures or stubbing.

Fixtures are just functions that take test function and run it with other necessary steps (before/after, wrap).

    (ns myapp.test
      (require [clojure.test :refer :all])

    (defn stub-current-thing [body]
      ;; with-redefs stubs things/current-thing function to return fixed
      ;; value for duration of each test
      (with-redefs [things/current-thing (fn [] {:foo :bar})]
        ;; run test body
        (body)))

    (use-fixtures :each stub-current-thing)

When used with `:once`, it wraps whole run of tests in current namespace with function

    (defn database-for-tests [all-tests]
      (setup-database)
      (all-tests)
      (drop-database))

    (use-fixtures :once database-for-tests)

## Grouping related tests with the testing macro
You can group related assertions in `deftest` unit tests within a context using the `testing` macro:

    (deftest add-nums 
      (testing "Positive cases"
        (is (= 2 (+ 1 1)))
        (is (= 4 (+ 2 2))))
      (testing "Negative cases"
        (is (= -1 (+ 2 -3)))
        (is (= -4 (+ 8 -12)))))

This will help clarify test output when run. Note that `testing` must occur inside a `deftest`.

## Defining a test with deftest
`deftest` is a macro for defining a unit test, similar to unit tests in other languages.

You can create a test as follows:

    (deftest add-nums
      (is (= 2 (+ 1 1)))
      (is (= 3 (+ 1 2))))

Here we are defining a test called `add-nums`, which tests the `+` function. The test has two assertions.

You can then run the test like this in your current namespace:

    (run-tests)

Or you can just run the tests for the namespace the test is in:

    (run-tests 'your-ns)

## are
The `are` macro is also part of the `clojure.test` library. It allows you to make multiple assertions against a template.

For example:

    (are [x y] (= x y)  
       4 (+ 2 2)
       8 (* 2 4))
    => true


Here, `(= x y)` acts as a template which takes each argument and creates an `is` assertion out of it.

This expands to multiple `is` assertions:

    (do 
      (is (= 4 (+ 2 2)))
      (is (= 8 (* 2 4))))

## Running tests with Leiningen
If you are using Leiningen and your tests are located in the test directory in your project root then you can run your tests using `lein test`

