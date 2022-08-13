---
title: "Closure Memoize Methods"
slug: "closure-memoize-methods"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
- closure.memoize()
- closure.memoizeAtMost(n)
- closure.memoizeAtLeast(n)
- closure.memoizeBetween(n, m)

Memoization is a method of caching the result of a closure invocation.
The memoize function applied to a closure returns a new closure whose return value is cached according to its input parameters. The caches used for the three tweaked variants of memoization methods are LRU caches, that is the least recently used element is removed from the cache first.

## Simple memoization
    def count = 0

    nonmemoized = { long n -> println "nonmemoized: $n"; count++ }

    nonmemoized(1)
    nonmemoized(2)
    nonmemoized(2)
    nonmemoized(1)
    assert count == 4


    def mcount = 0

    memoized = { long n -> println "memoized: $n"; mcount++ }.memoize()

    memoized(1)
    memoized(2)
    memoized(2)
    memoized(1)
    assert mcount == 2

