---
title: "Getting started with loops"
slug: "getting-started-with-loops"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Types of loops
A loop is a control flow structure to definitely or indefinitely run a set of statement written only once in code, until a certain condition is met or the process is terminated.

# Condition loops
These loops are repeated based on the state of their conditions.

## For loops
<!-- * _Main article: [For loops]_ -->

For loops are usually run upon a variable as the subject of iteration. For example, for loops can be run upon an integer to limit the number of times the loop should be run, or upon an array to iterate over it.

## While loops
<!-- * _Main article: [While loops]_ -->

While loops is the most basic type of condition loop that keeps running until its condition is changed (or until a `break` statement is executed).

## Variants
A variant of `while` loops is the `do... while` loop. It is the same as `while` loops, except that the content of the loop is run once before checking the condition.

Another variant is the `until`/`do... until` loops, which does the same as their counterparts in `while` except that they check the condition in the opposite way -- `while` loops run until the condition is false, and `until` loops run until the condition is true.

# Collection loops
These loops are repeated by iterating upon collections, such as arrays or iterables.

## Foreach loops
A `forEach` loop runs on a collection by executing the code once per item in collection, giving the value and/or the key of the item as parameter.

# Goto loops
`goto` loops are a set of statement between a label and a goto statement.

# Recursive loops
<!-- Main article: [Recursive loops] -->

In functional programming, recursive loops can be used to run a function recursively until a condition is met. This is a common cause for stack overflow errors.

