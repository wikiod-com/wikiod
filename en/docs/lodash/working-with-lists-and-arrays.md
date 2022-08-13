---
title: "Working with Lists and Arrays"
slug: "working-with-lists-and-arrays"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax

* _.map(collection, Function) => newCollection
* _.filter(collection, Predicate) => newCollection
* _.some(collection, Predicate) => true or false
* _.reduce(collection, BiFunction, seed) => accumulated value

## Parameters
| Parameter | Meaning |
| ------ | ------ |
| Collection | An iterable group of elements. This can be an array or an object. |
| Function | A function that takes 1 input, and returns one output. |
| BiFunction | A function that takes 2 inputs, and returns one output. |
| Predicate | A function that takes 1 input, and returns a boolean value. |
| seed | The initial value for a reduction operation. When this is left out, the first element of the collection is used instead. |

## Use _.map to Transform a List
`_.map` is useful for changing a list into a different list in a purely
declarative way.  Rather than using imperative techniques like a `while` or
`for` loop in javascript, you can just specify how you want to manipulate an
element of a list and 


Use `_.map` to make a new list transformed by the function you provide.

Let's say we want to square all the numbers in a list. First we'll create a list using the `_.range` function:

    var a = _.range(10);       // [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

Now we'll create a list of squares by using `_.map`:

    var b = _.map(a, function(e){ return e * e;} );
    // b is now [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]



## _.filter
Filtering a list down to only the elements we want to do.  Lodash provides a
function called `_.filter` filters elements based on the predicate function you
provide. A predicate is a function that takes data in and returns either true
or false.

Let's look at how we'd get just the even numbers from a list of the numbers 0 through 9:

    var numbers = _.range(0,10);      // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

    var evenNumbers = _.filter(numbers, function(e){ return e % 2 == 0; });
    // evenNumbers is now [ 0, 2, 4, 6, 8 ]


## _.some
We can assert some predicate over a collection using `_.some` to check if there
is at least one member of a collection that meets some criteria.  This is great
when writing business logic to assert certain conditions on a group of objects.
For example, let's say you wanted to make sure at least one person in a group
had a driver's license before it was possible for that group to go on a road
trip.  We make no guarantees on how happy of a group that'll be at the end of
the road trip, however.

    var friends = [
        {
            'name': 'Fred',
            'hasLicense': false
        },
        {
            'name': 'Steve',
            'hasGuitar': true
        },
        {
            'name': 'Mary',
            'hasLicense': true
        },
    ]
    
    function canGroupDrive(arr){
        return _.some(arr, function(e){ return e.hasLicense; });
    }
    
    canGroupDrive(friends);    // returns true



## _.reduce
Reducing a list to a single value is easy when you have `_.reduce`.  Let's say
we wanted to see if a group of people could afford a cab ride.  We'd want to
look at all the money they have together as a group, which means we'd want to
reduce a list of objects to a single value, in this case the sum of the money
they have.

    var friends = [
        {
            'name': 'Alice',
            'money': 10
        },
        {
            'name': 'Bob',
            'money': 3
        },
        {
            'name': 'Clyde',
            'money': 8
        },
    ]
    
    var totalMoney = function(arr){
        return _.reduce(
            arr,
            function(accumulated, e){
                return accumulated + e.money;
            },
            0
        );
    }
    
    function canAffordCab(arr){
        return 18 < totalMoney(arr);
    }
    
    canAffordCab(friends);    // returns true



