---
title: "Pro's & Con's of using Coffeescript"
slug: "pros--cons-of-using-coffeescript"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Pros
# Simplicity

Probably the best part of CoffeeScript is its simplicity. CoffeeScript allows for a more concise and simplistic syntax than plain JavaScript. One simple but surprisingly time-saving feature is that CoffeeScript has no need for `;` or `{}`, eliminating the need to spend hours finding out the place from which a `}` is missing. 

## Loops

Creating a loop that outputs the value of each item in an array unless the value is "monkey" in CoffeeScript is very easy. 

    animals = ["dog", "cat", "monkey", "squirrel"]
    for item in animals when item isnt "monkey"
        console.log item

in CoffeeScript compiles to

    var animals, i, item, len;

    animals = ["dog", "cat", "monkey", "squirrel"];

    for (i = 0, len = animals.length; i < len; i++) {
        item = animals[i];
        if (item !== "monkey") {
            console.log(item);
        }
    }

in JavaScript, but they both output

    dog
    cat
    squirrel

## String Interpolation

CoffeeScript: 

    "Hello, #{user}, how are you today?"

JavaScript: 

    "Hello, " + user + ", how are you today?";


