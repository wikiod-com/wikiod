---
title: "Collection Operators"
slug: "collection-operators"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Iterate over a collection
Lists
=====

    def lst = ['foo', 'bar', 'baz']
    // using implicit argument
    lst.each { println it }

    // using explicit argument
    lst.each { val -> println val }
    
    // both print:
    // foo
    // bar
    // baz

Iterate with index
------------------

    def lst = ['foo', 'bar', 'baz']
    // explicit arguments are required
    lst.eachWithIndex { val, idx -> println "$val in position $idx" }​​​​​​​​​​​​​​

    // prints:
    // foo in position 0
    // bar in position 1
    // baz in position 2


Maps
====
    def map = [foo: 'FOO', bar: 'BAR', baz: 'BAZ']
    
    // using implicit argument
    map.each { println "key: ${it.key}, value: ${it.value}"}
    
    // using explicit arguments
    map.each { k, v -> println "key: $k, value: $v"}

    // both print:
    // key: foo, value: FOO
    // key: bar, value: BAR
    // key: baz, value: BAZ

## Create a new list using collect
    def lst = ['foo', 'bar', 'baz']
    lst.collect { it } // ['foo', 'bar', 'baz']

    lst.collect { it.toUpperCase() } // ['FOO', 'BAR', 'BAZ']

To collect keys or values from a maps
-------------------------------------

    def map = [foo: 'FOO', bar: 'BAR', baz: 'BAZ']
    def keys = map.collect { it.key } // ['foo', 'bar', 'baz']
    def vals = map.collect { it.value } // ['FOO', 'BAR', 'BAZ']

The above example is equivalent to calling `map.keySet()` and `map.values()`

## Filter a  list with findAll
    def lst = [10, 20, 30, 40]

    lst.findAll { it > 25 } // [30, 40]

## Find the first element matching a condition
    def lst = [10, 20, 30, 40]
    
    lst.find { it > 25 } // 30. Note: it returns a single value

## Create maps with collectEntries
From lists

    def lst = ['foo', 'bar', 'baz']

    // for each entry return a list containing [key, value]
    lst.collectEntries { [it, it.toUpperCase()] } // [foo: FOO, bar: BAR, baz: BAZ]

    // another option, return a map containing the single entry
    lst.collectEntries { [(it): it.toUpperCase()] } // [foo: FOO, bar: BAR, baz: BAZ]

From maps

    def map = [foo: 'FOO', bar: 'BAR', baz: 'BAZ']

    map.collectEntries { [it.key*2, it.value*2] } // [foofoo: FOOFOO, barbar: BARBAR, bazbaz: BAZBAZ]

    // using explicit arguments k and v
    map.collectEntries { k, v -> [k*2, v*2] } // [foofoo: FOOFOO, barbar: BARBAR, bazbaz: BAZBAZ]

## Apply transformation to nested collections
Apply the transformation to non-collection entries, delving into nested collections too and preserving the whole structure.

    def lst = ['foo', 'bar', ['inner_foo', 'inner_bar']]

    lst.collectNested { it.toUpperCase() } // [FOO, BAR, [INNER_FOO, INNER_BAR]]

## Flatten a nested list
    def lst = ['foo', 'bar', ['inner_foo', 'inner_bar']]

    lst.flatten() ​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​​// ['foo', 'bar', 'inner_foo', 'inner_bar']

## Remove duplicates
    def lst = ['foo', 'foo', 'bar', 'baz']

    // *modifies* the list removing duplicate items
    lst.unique() // [foo, bar, baz]

    // setting to false the "mutate" argument returns a new list, leaving the original intact
    lst.unique(false) // [foo, bar, baz]

    // convert the list to a Set, thus removing duplicates
    lst.toSet() // [baz, bar, foo]

    // defining a custom equality criteria. For example: to elements are equal if have the same first letter
    println lst.unique() { it[0] } // [foo, bar]. 'bar' and 'baz' considered equal

## Build a map from two lists
    nrs = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    lets = ['a', 'b', 'c', 'd', 'e', 'f']

    println GroovyCollections.transpose([nrs, lets])
            .collect {le -> [(le[0]):le[1]]}.collectEntries { it }

    or
       
    println [nrs,lets].transpose().collectEntries{[it[0],it[1]]}

    // [1:a, 2:b, 3:c, 4:d, 5:e, 6:f]   

