---
title: "Series"
slug: "series"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Series are the core of how Rebol works and as such understanding how to manipulate series is fundamental in mastering the language. There are two series subtypes—blocks and strings: both share essential common traits with regard to traversal, alteration and extraction, though all series have subtle differences that affect how they are interpreted.

## Moving the last item of the block to the beginning
This one-liner illustrates some of the basic series operations.

Before analysing, we'll initiate a block to be manipulated:

    stuff: [chair lamp table book carpet]

Now our one-liner:

    head insert stuff take back tail stuff

There are five operations within this example, and with each of them it's important to understand that Rebol functions usually return useful values. This allows the functions to be chained into a relatively concise statement.

We'll perform this analysis from right to left:

# Tail Stuff

Rebol block series are similar to arrays in other languages, they're abstract collection with *x* number of values. However they can also be referenced at various positions—anywhere between the head (before the first value) and the tail (after the last value).

The native `tail` will return the block at its tail.

# Back (Tail Stuff)

Similarly `back` will return the block immediately before its current position. In this case, being at the tail, `back` will return the block at the penultimate position.

# Take (Back Tail Stuff)

`take` performs two operations—it returns the value immediately after the current position, and simultaneously removes it from the block.

# Insert Stuff (Take Back Tail Stuff)

`insert` places a value into the block at the current position and returns the block immediately after the placed value. Note that `stuff` retains it's position at the beginning of the block—at no point is it reassigned another position—the latter part of the statement (taking the value) is self-contained.

# Head (Insert ...)

And finally `head` returns the beginning of the block. Although the `insert` here was used at the beginning of the block, it returns the block *one position after* the beginning after the value is placed.

