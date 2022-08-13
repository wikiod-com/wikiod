---
title: "true, false and  commands"
slug: "true-false-and--commands"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
* true, : - always return 0 as exit code.
* false - always returns 1 as exit code.

## Infinite Loop
    while true; do
        echo ok
    done

or
    
    while :; do
       echo ok
    done
or
    
    until false; do
        echo ok
    done

## Function Return
    function positive() {
        return 0
    }

    function negative() {
        return 1
    }

## Code that will always/never be executed
    if true; then
        echo Always executed
    fi
    if false; then
        echo Never executed
    fi

