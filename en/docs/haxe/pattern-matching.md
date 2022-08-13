---
title: "Pattern matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Pattern matching is the process of branching depending on provided patterns. All pattern matching is done within a `switch` expression, and individual `case` expressions represent the patterns.

The fundamental rules of pattern matching are:

 - patterns will always be matched from top to bottom;
 - the topmost pattern that matches the input value has its expression executed;
 - a `_` pattern matches anything, so `case _:` is equal to `default:`.

When all possible cases are handled, the catch-all `_` pattern or `default` case is not required.

## Structure matching
Assume the following structure:

    var dog = {
        name : "Woofer",
        age : 7
    };

Enum matching can be performed as follows:

    var message = switch(dog) {
        case { name : "Woofer" }:
            "I know you, Woofer!";
        case _:
            "I don't know you, sorry!";
    }

## References

 - ["Structure matching", Haxe manual][1]


  [1]: https://haxe.org/manual/lf-pattern-matching-structure.html

## Guards
It is also possible to further restrict patterns with guards. These are defined by the `case ... if(condition):` syntax.

    var myArray = [7, 6];
    var s = switch(myArray) {
        case [a, b] if (b > a):
            b + ">" +a;
        case [a, b]:
            b + "<=" +a;
        case _: "found something else";
    }

## References

 - ["Guards", Haxe manual][1]


  [1]: https://haxe.org/manual/lf-pattern-matching-guards.html

## Enum matching
Assume the following enum:

    enum Operation {
        Multiply(left : Int, right : Int);
    }

Enum matching can be performed as follows:

    var result = switch(Multiply(1, 3)) {
        case Multiply(_, 0):
            0;
        case Multiply(0, _):
            0;
        case Multiply(l, r):
            l * r;
    }

## References

 - ["Enum matching", Haxe manual][1]


  [1]: https://haxe.org/manual/lf-pattern-matching-enums.html

## Array matching
    var result = switch([1, 6]) {
        case [2, _]:
            "0";
        case [_, 6]:
            "1";
        case []:
            "2";
        case [_, _, _]:
            "3";
        case _:
            "4";
    }

## References

 - ["Array matching", Haxe manual][1]


  [1]: https://haxe.org/manual/lf-pattern-matching-array.html

## Or patterns
The `|` operator can be used anywhere within patterns to describe multiple accepted patterns. If there is a captured variable in an or-pattern, it must appear in both its sub-patterns.

    var match = switch(7) {
        case 4 | 1: "0";
        case 6 | 7: "1";
        case _: "2";
    }

## References

 - ["Or patterns", Haxe manual][1]


  [1]: https://haxe.org/manual/lf-pattern-matching-or.html

## Extractors
Extractors are identified by the `extractorExpression => match` expression. Extractors consist of two parts, which are separated by the `=>` operator.

 1. The left side can be any expression, where all occurrences of underscore `_` are replaced with the currently matched value.
 2. The right side is a pattern which is matched against the result of the evaluation of the left side.

Since the right side is a pattern, it can contain another extractor. The following example "chains" two extractors:

    static public function main() {
        switch(3) {
            case add(_, 1) => mul(_, 3) => a:
                trace(a); // mul(add(3 + 1), 3)
        }
    }
    
    static function add(i1:Int, i2:Int) {
        return i1 + i2;
    }

    static function mul(i1:Int, i2:Int) {
        return i1 * i2;
    }

It is currently not possible to use extractors within or-patterns. However, it is possible to have or-patterns on the right side of an extractor.

## References

 - ["Extractors", Haxe manual][1]


  [1]: https://haxe.org/manual/lf-pattern-matching-extractors.html

