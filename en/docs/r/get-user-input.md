---
title: "Get user input"
slug: "get-user-input"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
- variable <- readline(prompt = "Any message for user")

- name <- readline(prompt = "What's your name")

## User input in R
Sometimes it can be interesting to have a cross-talk between the user and the program, one example being the [swirl](http://swirlstats.com/) package that had been designed to teach R in R.

One can ask for user input using the `readline` command:

    name <- readline(prompt = "What is your name?")

The user can then give any answer, such as a number, a character, vectors, and scanning the result is here to make sure that the user has given a proper answer. For example:

    result <- readline(prompt = "What is the result of 1+1?")
    while(result!=2){
        readline(prompt = "Wrong answer. What is the result of 1+1?")
    }

However, it is to be noted that this code be stuck in a never-ending loop, as user input is saved as a character.

We have to coerce it to a number, using `as.numeric`:

    result <- as.numeric(readline(prompt = "What is the result of 1+1?"))
    while(result!=2){
        readline(prompt = "Wrong answer. What is the result of 1+1?")
    }


