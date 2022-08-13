---
title: "Pattern Matching with core.match"
slug: "pattern-matching-with-corematch"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

The `core.match` library implements a pattern match compilation algorithm that uses the notion of "necessity" from lazy pattern matching.

## Matching Literals
    (let [x true
          y true
          z true]
      (match [x y z]
         [_ false true] 1
         [false true _ ] 2
         [_ _ false] 3
         [_ _ true] 4))

    ;=> 4

## Matching a Vector
    (let [v [1 2 3]]
      (match [v]
        [[1 1 1]] :a0
        [[1 _ 1]] :a1
        [[1 2 _]] :a2))  ;; _ is used for wildcard matching

    ;=> :a2

## Matching a Map
    (let [x {:a 1 :b 1}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b _}] :a1
         [{:x 3 :y _ :z 4}] :a2))

    ;=> :a1

## Matching a literal symbol
    (match [['asymbol]]
      [['asymbol]] :success)

    ;=> :success

