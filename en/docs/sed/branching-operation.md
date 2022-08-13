---
title: "Branching Operation"
slug: "branching-operation"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The branching operation of `sed` can help control the flow of the program.

## Do multiple line regexp replacing with unconditional branch
Assume that I have a file named `in.txt`:

    $ cat in.txt
    a
    b
    a
    c
    a
    d

I only want to replace the `a\nc` with `deleted`, but not `a\nb` or `a\nd`.

    $ sed -e ':loop         # create a branch/label named `loop`
     $!{
     N                      # append the next line of input into the pattern space
     /\n$/!b loop           # If it is not the last line go to the `loop` branch again
     }
     s/a\nc/"deleted"/' in.txt    # do replacing in the pattern space

    a
    b
    "deleted"    # see! succeed
    a
    d



