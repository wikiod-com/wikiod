---
title: "Named capture groups"
slug: "named-capture-groups"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
 - Build a named capture group (`X` being the pattern you want to capture):

    (?'name'X)
    (?<name>X)
    (?P<name>X)

 - Reference a named capture group:

    ${name}
    \\{name}
    g\\{name}

Python and Java don't allow multiple groups to use the same name.

## What a named capture group looks like
Given the flavors, the named capture group may looks like this:

    (?'name'X)
    (?<name>X)
    (?P<name>X)

With `X` being the pattern you want to capture. Let's consider the following string:

>Once upon a time there was a *pretty little girl*...
>
>Once upon a time there was a *unicorn with an hat*...
>
>Once upon a time there was a *boat with a pirate flag*...

In which I want to capture the subject (in *italic)* of every lines. I'll use the following expression `.* was a (?<subject>[\w ]+)[.]{3}`.

The matching result will hold:

    MATCH 1
    subject    [29-47]    `pretty little girl`
    MATCH 2
    subject    [80-99]    `unicorn with an hat`
    MATCH 3
    subject    [132-155]    `boat with a pirate flag`

## Reference a named capture group
As you may (or not) know, you can reference a capture group with:

    $1

`1` being the group number.

In the same way, you can reference a named capture group with:

    ${name}
    \{name}
    g\{name}

Let's take the preceding example and replace the matches with

    The hero of the story is a ${subject}.

The result we will obtain is:

    The hero of the story is a pretty little girl. 
    The hero of the story is a unicorn with an hat. 
    The hero of the story is a boat with a pirate flag.

