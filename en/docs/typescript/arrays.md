---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Finding Object in Array
# Using find()
 
    const inventory = [
        {name: 'apples', quantity: 2},
        {name: 'bananas', quantity: 0},
        {name: 'cherries', quantity: 5}
    ];

    function findCherries(fruit) { 
        return fruit.name === 'cherries';
    }

    inventory.find(findCherries); // { name: 'cherries', quantity: 5 }

    /* OR */

    inventory.find(e => e.name === 'apples'); // { name: 'apples', quantity: 2 }    


