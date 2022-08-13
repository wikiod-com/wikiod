---
title: "Chaining"
slug: "chaining"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Implicit chaining with `_(arr1)` and explicit chaining with `_.chain(arr1)` work in similar ways. The examples below show how they differ slighlty.

## Explicit chaining with `_.chain(...)`

    var arr1 = [10, 15, 20, 25, 30, 15, 25, 35];

    var sumOfUniqueValues = _.chain(arr1)
        .uniq()
        .sum()       // sum returns a single value
        .value();    //   which must be unwrapped manually with explicit chaining

    // sumOfUniqueValues is now 135


## Implicit chaining with `_(...)`

    var arr1 = [10, 15, 20, 25, 30, 15, 25, 35];

    var sumOfUniqueValues = _(arr1)
        .uniq()
        .sum();      // sum returns a single value and is automatically unwrapped
                     //   with implicit chaining

    // sumOfUniqueValues is now 135


The two behave differently when ending the chain with an operation that returns a single value: With implicit chaining, the "unwrapping" of the single value is implied. (Thus no need to call `.value()`.)

(When the implicit chain ends with a collection value, you'll still need to unwrap the result with `.value()`.)





## Chaining
Any lodash collection method has two syntaxes.

Without chaining:

    var arr1 = [10, 15, 20, 25, 30, 15, 25, 35];

    var arr2 = _.filter(arr1, function(item){ return item % 10 === 5 });
    // arr2 now contains [15, 25, 15, 25, 35]

    var arr3 = _.uniq(arr2);
    // arr3 now contains [15, 25, 35]

    var arr4 = _.map(arr3, function(item){ return item + 1 });
    // arr4 now contains [16, 26, 36]

With chaining:

    var arr1 = [10, 15, 20, 25, 30, 15, 25, 35];

    var arr4 = _(arr1)
        .filter(function(item){ return item % 10 === 5 })
        .uniq()
        .map(function(item){ return item + 1 })
        .value();
    // arr4 now contains [16, 26, 36] without creating the intermediate results.

The chaining version of this is actually more efficient, since no intermediate results are created. The expressions are evaluated lazily by the call to `.values()` at the end of the chain.


