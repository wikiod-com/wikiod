---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9881
type: docs
toc: true
---

## Syntax
- for (*initialization*; *condition*; *final_expression*) { }
- for (*key* in *object*) { }
- for (*variable* of *iterable*) { }
- while (*condition*) { }
- do { } while (*condition*)
- for each (*variable* in *object*) { } // ECMAScript for XML

Loops in JavaScript typically help solve problems which involve repeating specific code *x* amount of times. Say you need to log a message 5 times. You could do this:

    console.log("a message");
    console.log("a message");
    console.log("a message");
    console.log("a message");
    console.log("a message");

But that's just time-consuming and kind of ridiculous. Plus, what if you needed to log over 300 messages? You should replace the code with a traditional "for" loop:

    for(var i = 0; i < 5; i++){
        console.log("a message");
    }

## Standard "for" loops
### Standard usage

    for (var i = 0; i < 100; i++) {
        console.log(i);
    }

Expected output:

> 0  
> 1  
> ...  
> 99

### Multiple declarations

Commonly used to cache the length of an array.

    var array = ['a', 'b', 'c'];
    for (var i = 0; i < array.length; i++) {
        console.log(array[i]);
    }

Expected output:

> 'a'  
> 'b'    
> 'c'

### Changing the increment

    for (var i = 0; i < 100; i += 2 /* Can also be: i = i + 2 */) {
        console.log(i);
    }

Expected output:

> 0  
> 2    
> 4  
> ...  
> 98

### Decremented loop

    for (var i = 100; i >=0; i--) {
        console.log(i);
    }

Expected output:

> 100  
> 99    
> 98  
> ...  
> 0

## "for ... of" loop
<!-- if version [gte 6] -->

    const iterable = [0, 1, 2];
    for (let i of iterable) {
        console.log(i);
    }

Expected output:

> 0  
> 1  
> 2

 The advantages from the for...of loop are:

 - This is the most concise, direct syntax yet for looping through array elements
 - It avoids all the pitfalls of for...in
 - Unlike `forEach()`, it works with break, continue, and return

# Support of for...of in other collections

## Strings

for...of will treat a string as a sequence of Unicode characters:

    const string = "abc";
    for (let chr of string) {
      console.log(chr);
    }

Expected output:

> a
> b
> c

## Sets

for...of works on [Set objects](https://www.wikiod.com/javascript/set).

**Note**:
- A Set object will eliminate duplicates.
- Please [check this reference](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Set#Browser_compatibility) for `Set()` browser support.


    const names = ['bob', 'alejandro', 'zandra', 'anna', 'bob'];
    
    const uniqueNames = new Set(names);
    
    for (let name of uniqueNames) {
      console.log(name);
    }

Expected output:

> bob  
> alejandro  
> zandra  
> anna

## Maps

You can also use for...of loops to iterate over [Map](https://www.wikiod.com/javascript/map)s. This works similarly to arrays and sets, except the iteration variable stores both a key and a value.

    const map = new Map()
      .set('abc', 1)
      .set('def', 2)

    for (const iteration of map) {
      console.log(iteration) //will log ['abc', 1] and then ['def', 2]
    }

You can use [destructuring assignment](https://www.wikiod.com/javascript/destructuring-assignment#Destructuring Arrays) to capture the key and the value separately:

    const map = new Map()
      .set('abc', 1)
      .set('def', 2)

    for (const [key, value] of map) {
      console.log(key + ' is mapped to ' + value)
    }
    /*Logs:
      abc is mapped to 1
      def is mapped to 2
    */

## Objects

for...of loops *do not* work directly on plain Objects; but, it is possible to iterate over an object’s properties by switching to a for...in loop, or using [`Object.keys()`](https://www.wikiod.com/javascript/objects#Object.keys):

    const someObject = { name: 'Mike' };
    
    for (let key of Object.keys(someObject)) {
      console.log(key + ": " + someObject[key]);
    }

Expected output:

> name: Mike

<!-- end version if -->

## "for ... in" loop
> **Warning**<br/>
> for...in is intended for iterating over object keys, not array indexes. [Using it to loop through an array is generally discouraged](http://stackoverflow.com/questions/500504/why-is-using-for-in-with-array-iteration-such-a-bad-idea). It also includes properties from the prototype, so it may be necessary to check if the key is within the object using `hasOwnProperty`. If any attributes in the object are defined by the `defineProperty/defineProperties` method and set the param `enumerable: false`, those attributes will be inaccessible.

    var object = {"a":"foo", "b":"bar", "c":"baz"};
    // `a` is inaccessible
    Object.defineProperty(object , 'a', {
            enumerable: false,
    });
    for (var key in object) {
        if (object.hasOwnProperty(key)) {
          console.log('object.' + key + ', ' + object[key]);
        }
    }

Expected output:

> object.b, bar  
> object.c, baz

## "while" Loops
## **Standard While Loop**
A standard while loop will execute until the condition given is false:

    var i = 0;
    while (i < 100) {
        console.log(i);
        i++;
    }
Expected output:

> 0  
> 1  
> ...  
> 99

### Decremented loop

    var i = 100;
    while (i > 0) {
        console.log(i);
        i--; /* equivalent to i=i-1 */
    }

Expected output:

> 100  
> 99    
> 98  
> ...  
> 1

## **Do...while Loop**
A do...while loop will always execute at least once, regardless of whether the condition is true or false:

    var i = 101;
    do {
        console.log(i);
    } while (i < 100);

Expected output:

> 101  


## "continue" a loop
## Continuing a "for" Loop

When you put the `continue` keyword in a for loop, execution jumps to the update expression (`i++` in the example):

    for (var i = 0; i < 3; i++) {
        if (i === 1) {
            continue;
        }
        console.log(i);
    }

Expected output:

> 0  
> 2

## Continuing a While Loop

When you `continue` in a while loop, execution jumps to the condition (`i < 3` in the example):

    var i = 0;
    while (i < 3) {
        if (i === 1) {
            i = 2;
            continue;
        }
        console.log(i);
        i++;
    }

Expected output:

> 0  
> 2


## "do ... while" loop
```
var availableName;
do {
    availableName = getRandomName();
} while (isNameUsed(name));
```

A `do while` loop is guaranteed to run at least once as it's condition is only checked at the end of an iteration. A traditional `while` loop may run zero or more times as its condition is checked at the beginning of an iteration.

## Break specific nested loops
We can name our loops and break the specific one when necessary.
    
    outerloop:
    for (var i = 0;i<3;i++){
        innerloup:
        for (var j = 0;j <3; j++){
            console.log(i);
            console.log(j);
            if (j == 1){
                break outerloop;    
            }
        }
    }

Output:

    0
    0
    0
    1




## Break and continue labels
Break and continue statements can be followed by an optional label which works like some kind of a goto statement, resumes execution from the label referenced position

    for(var i = 0; i < 5; i++){
      nextLoop2Iteration:
      for(var j = 0; j < 5; j++){
        if(i == j) break nextLoop2Iteration;
        console.log(i, j);
      }
    }

> ***i=0 j=0 skips rest of j values***  
>1 0  
> ***i=1 j=1 skips rest of j values***  
>2 0  
>2 1
> ***i=2 j=2 skips rest of j values***  
>3 0  
>3 1  
>3 2  
> ***i=3 j=3 skips rest of j values***  
>4 0  
>4 1  
>4 2  
>4 3  
> ***i=4 j=4 does not log and loops are done***  

## "Break" out of a loop
## Breaking out of a while loop

    var i = 0;
    while(true) {
        i++;
        if(i === 42) {
            break;
        }
    }
    console.log(i);

Expected output:

> 42  


## Breaking out of a for loop

    var i;
    for(i = 0; i < 100; i++) {
        if(i === 42) {
            break;
        }
    }
    console.log(i);

Expected output:

> 42  


