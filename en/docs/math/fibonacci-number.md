---
title: "Fibonacci Number"
slug: "fibonacci-number"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Fibonacci Numbers is the integer sequence ([OEIS A000045](https://oeis.org/A000045)) F(n) that obeys the following recurrence:

` F(n) = F(n-1) + F(n-2) `


For F(0) = 0, F(1) = 1, the series thus formed is 0, 1, 1, 2, 3, 5, 8, 13, 21, ...


Fibonacci numbers appear in several areas of Discrete Mathematics and Algorithms.

## Naive Recursive Implementation
Fibonacci numbers are used as a very common example for teaching recursion.

<!-- language: haskell -->

    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

