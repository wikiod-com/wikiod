---
title: "Assigning strings with repeated characters"
slug: "assigning-strings-with-repeated-characters"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

There are times you need to assign a string variable with a specific character repeated a specific number of times. VBA provides two main functions for this purpose:
- `String`/`String$`
- `Space`/`Space$`.


## Use the String function to assign a string with n repeated characters
    Dim lineOfHyphens As String
    'Assign a string with 80 repeated hyphens
    lineOfHyphens = String$(80, "-")


## Use the String and Space functions to assign an n-character string
    Dim stringOfSpaces As String

    'Assign a string with 255 repeated spaces using Space$
    stringOfSpaces = Space$(255)

    'Assign a string with 255 repeated spaces using String$
    stringOfSpaces = String$(255, " ")


