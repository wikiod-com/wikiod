---
title: "Typealias"
slug: "typealias"
draft: false
images: []
weight: 9854
type: docs
toc: true
---

## typealias for closures with parameters
    typealias SuccessHandler = (NSURLSessionDataTask, AnyObject?) -> Void

This code block creates a type alias named `SuccessHandler`, just in the same way `var string = ""` creates a variable with the name `string`.

Now whenever you use `SuccessHandler`, for example:

    func example(_ handler: SuccessHandler) {}

You are essentilly writing:

    func example(_ handler: (NSURLSessionDataTask, AnyObject?) -> Void) {}

## typealias for empty closures
    typealias Handler = () -> Void
    typealias Handler = () -> ()

This block creates a type alias that works as a Void to Void function (takes in no parameters and returns nothing).

Here is a usage example:

    var func: Handler?

    func = {}

## typealias for other types
    typealias Number = NSNumber

You can also use a type alias to give a type another name to make it easier to remember, or make your code more elegant.

**typealias for Tuples** 

    typealias PersonTuple = (name: String, age: Int, address: String)
And this can be used as:

    func getPerson(for name: String) -> PersonTuple {
        //fetch from db, etc
        return ("name", 45, "address")
    }

