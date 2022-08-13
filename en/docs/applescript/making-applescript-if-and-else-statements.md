---
title: "Making Applescript If and Else Statements"
slug: "making-applescript-if-and-else-statements"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This topic is about if and else statements.

## if variable = 2
    set var to 2
    
    if var = 2 then
        say "Var equals 2"
    end if

## if var1 = 4, else statements
    set var1 to 5
    //set the number to anything

    if var1 = 5 then
        say "Var one equals 5"
    else
     say "Var one does not equal 5"
    end if

## Dialogue returned text
    display dialog "Password" default answer ""
    set w to text returned of the result
    if w = "Password" then
        display notification "Correct"
    end if

