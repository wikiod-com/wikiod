---
title: "For loops"
slug: "for-loops"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
* for(init; condition; increment){ content_code(); } // general syntax
* for(int i = 0; i < numberRuns; ++i){ actions_with(i); } // run an action for a numberRuns times
* for(int i = 0; i < sizeof(array); ++i){ actions_with(array[i]); } // iteration over an array

## General for loop
Most programming languages support the `for`-loop control structure.

It is generally implemented in this way:

    for(init; condition; increment){
        content_code();
    }

The above pseudocode is identical with the pseudocode below:

    init;
    start_loop:
    if(condition){
        content_code();
        increment;
        goto start_loop;
    }

This shows that:
* `init` is run before the loop, used to initialize things for running that loop
  * In some programming languages like Java, variables can be _declared_ in `init`, and the scope of the declared variables will be limited to that loop.
* `condition` is a condition to determine when the loop can be run. If this evaluates to `false`, the loop will stop executing.
* `increment` is usually a statement used to manipulate parameters used in `condition`, so when `increment` is run a certain number of times, `condition` becomes `false` and the loop breaks.
* `content_code()` is the core code to be run within the loop.

