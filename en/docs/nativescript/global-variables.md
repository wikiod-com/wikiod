---
title: "Global Variables"
slug: "global-variables"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Console
NativeScript’s global `console` variable lets you print values to your terminal for debugging. The simplest usage is passing a value to the `console.log()` function:

```JavaScript
console.log("hello world");
```

The `console` object has several other methods, including `dump()`, `trace()`, `assert()` and [more](https://docs.nativescript.org/cookbook/console).

``` JavaScript
// Prints the state of a full object.
console.dump({ firstName: "Native", lastName: "Script"}); 

// Prints the current stack trace
console.trace();

// Asserts a boolean condition, and prints to the console if the assertion fails.
console.assert(1 === 1, "This won’t print as the condition is true");
console.assert(1 === 2, "This will print as the condition is false"); 

## Timer (JavaScript)
NativeScript's global `timer` variable lets you set timeouts and intervals for asynchronous delayed function calls. 

Importing

    var timer = require("timer")

Timeouts

    var callback = function(){
        console.log("I will be executed once after 500ms");
    }
    var timeoutId = timer.setTimeout(callback, 500);
    
    // clearing the timeout
    timer.clearTimeout(timeoutId);

Intervals

    var callback = function(){
        console.log("I will be executed every 500 ms")
    }
    var intervalId = timer.setInterval(callback, 500);
    
    // clearing the interval
    timer.clearInterval(intervalId);

