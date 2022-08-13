---
title: "Patterns and Actions"
slug: "patterns-and-actions"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Introduction
An `awk` consists of patterns and actions, enclosed in curly brackets, to be taken if a pattern matches. The most basic pattern is the empty pattern, which matches any record. The most basic action is the empty action, which is equivalent to `{ print }`, which is, in turn, equivalent to `{ print $0 }`. If both the pattern and the action are empty, `awk` will simply do nothing.

The following program will simply echo its input, for example:

    awk '{ print }' /etc/passwd

Since `{ print }` is the default action, and since a true value matches any record, that program could be re-written as:

    awk '1' /etc/passwd

The most common type of pattern is probably a regular expression enclosed in slashes. The following program will print all records that contain at least two subsequent occurrences of the letter `o`, for example:

    awk '/oo+/ { print }' /etc/passwd

However, you can use arbitrary expressions as patterns. The following program prints the names (field one) of users in group zero (field four), for example:

    awk -F: '$4 == 0 { print $1 }' /etc/passwd

Instead of matching exactly, you can also match against a regular expression. The following program prints the names of all users in a group with at least one zero in its group id:

    awk -F: '$4 ~ /0/ { print $1 }' /etc/passwd


## Filter Lines by length
> This pattern will allow you to filter lines depending on its length

    $cat file
    AAAAA
    BBBB
    CCCC
    DDDD
    EEEE
    
    $awk 'length($0) > 4 { print $0 }' file
    AAAAA
    $

Anyway, the pattern will allow the next code block to be executed, then, as the **default action for AWK is printing the current line** `{print}`, we´ll see the same result when executing this:

    $awk 'length($0) > 4 ' file
    AAAAA


