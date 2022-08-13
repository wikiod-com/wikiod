---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## map
The `.map` function accepts an array and an iteratee function, the iteratee produces a transformed copy of each array object.

The iteratee function provides 3 arguments
1. `item` - The current iterated object
2. `i` - The index of the iterated object
3. `list` - A reference to the original array/list

The new Array will be of the same length as the old array but will hold the trasformed objects 

**Example:**

    _.map([1, 2, 3, 4], function(item, i, list) {
        return (item*item);
    });
    // [1, 4, 9, 16]

A more concise way to write the above example using ES6 would be 

    _.map([1, 2, 3, 4], (item, i, list) => {
        return (item*item);
    });

or using an inline lambda expression

    _.map([1, 2, 3, 4], (item, i, list) => (item*item));

Map is also useful when you want to pluck properties from objects and make an array of them

**Example:**

    let people = [{name: 'he-man', age: 22}, {name: 'man-at-arms', age: 44}];

    _.map(people, function(item) {
        return item.name;
    });
    // ['he-man', 'man-at-arms']

## each
The `_.each` function accepts an array or an object, an iteratee function and an optional `context` object, the iteratee function is invoked once and in order for each array item
The iteratee function provides 3 arguments

    item - The current iterated object (or value if an object was passed)
    i - The index of the iterated object (or key if an object was passed)
    list - A reference to the original array/list (the object that was passed)

**Example 1:**

    _.each(["hello", "underscore"], function(item, i, list) {
        alert(item)
    });

The above example will show 2 alerts, the first with the words "hello" and the second for "world".

**Example 2:**

    _.each({one: 1, two: 2, three: 3}, (value, key, object) => 
        console.log(JSON.stringify(object));
    );

This example will log a stringified version of the object 3 times.

`.each` is a terminal operation, and unlike other intermediate functions (map, pluck, values etc..) you don't need to return inside the iteratee function body. 

