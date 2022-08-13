---
title: "Defining functions with list arguments"
slug: "defining-functions-with-list-arguments"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Function and Call
Lists as arguments are just another variable:

    def func(myList):
        for item in myList:
            print(item)

and can be passed in the function call itself:

    func([1,2,3,5,7])

    1
    2
    3
    5
    7

Or as a variable:

    aList = ['a','b','c','d']
    func(aList)

    a
    b
    c
    d

