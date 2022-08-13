---
title: "Branching"
slug: "branching"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
 - if (*condition*) { ... }
 - if (*condition*) { ... } else { ... }
 - if (*condition*) { ... } else if (*condition*) { ... } else { ... }
 - // Braces are optional for single line statements  
    if (*condition*)  ... else if (*condition*) ... else ... 
 - switch (*expression*) { case *pattern*: ... default: ... }
 - *condition* ? *expression if true* : *expression if false*;

All branching expressions make it possible to return evaluated expressions. This means branching results can be assigned to variables. In this case, **all expressions that can be evaluated by a successful condition test must pass type unification**. If no `else` expression is given, the type is inferred to be `Void`.

## If / else if / else
    if (a > b) {
        trace("You win!");
    } else if (a == b) {
        trace("It's a draw!");
    } else {
        trace("You lose!");
    }

    // Assigning the evaluated expression to a variable
    var message =  if (a > b) {
        "You win!";
    } else if (a == b) {
        "It's a draw!";
    } else {
        "You lose!";
    }
    trace(message);   

## Reference

 - ["If", Haxe manual][1]


  [1]: https://haxe.org/manual/expression-if.html

## Ternary operator
    n % 2 == 0 ? trace("n is even!") : trace("n is odd!");

    // Assigning the evaluated expression to a variable
    var message = n % 2 == 0 ? "n is even!" : "n is odd!";
    trace(message);


## Reference

 - ["If", Haxe manual][1]


  [1]: https://haxe.org/manual/expression-if.html

## Switch
    switch (n % 2) {
        case 0: trace("n is even!");
        case 1: trace("n is odd!");
        default: trace("I don't know!");
    }

    // Assigning the evaluated expression to a variable
    var message = switch (n % 2) {
        case 0: "n is even!";
        case 1: "n is odd!";
        default: "I don't know!";
    }
    trace(message);

Note that **`case` body expressions never fall through**, so using the `break` expression in this context isn't supported by Haxe.

## Reference:

 - ["Switch", Haxe manual][1]


  [1]: https://haxe.org/manual/expression-switch.html

