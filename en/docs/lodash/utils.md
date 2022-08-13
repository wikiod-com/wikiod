---
title: "Utils"
slug: "utils"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## _.identity
This method just returns the first argument.

    var res1 = _.identity(10, 20);
    // res1 now is 10

    var res2 = _.identity("hello", "world");
    // res2 now is "hello"

# What does *_.identity* mean in lodash documentation?

This method is used throughout *lodash* documentation instead of `function(x){return x;}` (or ES6 equivalent `x => x`).

It usually means either "no transformation" or when used as a predicate: the truthiness of the value.

## Example of *_.identity* in documentation of *_.times*

The `_.times` function takes two arguments. Its expressed like this in the documentation: *var res = _.times(n, [iteratee=_.identity])*

The *iteratee* is used to transform the values as they are iterated over.

The documentation shows that the *iteratee* parameter is optional, and if it is omitted it will have the default value of `_.identity`, **which in this case means "no transformation"**

    var res = _.times(5);      // returns [0, 1, 2, 3, 4]

    // means the same as:
    var res = _.times(5, _.identity);

    // which again does the same as:
    var res = _.times(5, function(x){ return x; });

    // or with the ES6 arrow syntax:
    var res = _.times(5, x => x);

## Example of *_.identity* in documentation of *_.findKey* and *_.findLastKey*

The `_.findKey` and `_.findLastKey` functions takes two arguments. Its expressed like this in the documentation: *_.findKey(object, [predicate=_.identity])* and *_.findLastKey(object, [predicate=_.identity])*

This again means that the second parameter is optional, and if it is omitted it will have the default value of `_.identity`, **which in this case means the first (or last) of "anything that is truthy"**

    var p = {
        name: "Peter Pan",
        age: "Not yet a grownup",
        location: "Neverland"
    };

    var res1 = _.findKey(p);        // returns "name"
    var res2 = _.findLastKey(p);    // returns "location"

    // means the same as:
    var res1 = _.findKey(p, _.identity);
    var res2 = _.findLastKey(p, _.identity);

    // which again means the same as:
    var res1 = _.findKey(p, function(x) { return x; }
    var res2 = _.findLastKey(p, function(x) { return x; }

    // or with ES6 arrow syntax:
    var res1 = _.findKey(p, x => x);
    var res2 = _.findKey(p, x => x);



