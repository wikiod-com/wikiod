---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - for (*variable identifier* in *iterating collection*) { *expression* }
 - while (*condition*) { *expression* }
 - do { *expression* } while (*condition*);
 - break;
 - continue;

## For
[For-loops][1] iterate over an **iterating collection**. An iterating collection is any class which structurally unifies with `Iterator<T>` or `Iterable<T>` types from the Haxe standard library.

A for-loop which logs numbers in range 0 to 10 (exclusive) can be written as follows:

    for (i in 0...10) {
        trace(i);
    }

The variable identifier `i` holds the individual value of elements in the iterating collection. This behaviour is similar to for-each in other languages.

A for-loop which logs elements in an array can therefore be written as follows:

    for (char in ['a', 'b', 'c', 'd']) {
        trace(char);
    }

Try the example on [try.haxe.org][2].

## References

 - ["For", Haxe manual][1]
 - ["Iterators", Haxe manual][3]


  [1]: https://haxe.org/manual/expression-for.html
  [2]: http://try.haxe.org/#ba0c2
  [3]: http://haxe.org/manual/lf-iterators.html

## While
[While-loops][1] execute a body expression as long as the loop condition evaluates to `true`.

A while-loop which logs numbers in range 9 to 0 (inclusive) can be written as follows:

    var i = 10;
    while (i-- > 0) {
        trace(i);
    }

Try the example on [try.haxe.org][2].

## References

 - ["While", Haxe manual][1]


  [1]: https://haxe.org/manual/expression-while.html
  [2]: http://try.haxe.org/#d58eA

## Do-while
[Do-while-loops][1] execute a body expression at least once, and then keep executing it as long as the loop condition evaluates to `true`.

A do-while-loop which logs numbers in range 10 to 0 (inclusive) can be written as follows:

    var i = 10;
    do {
        trace(i);
    } while (i-- > 0);

Try the example on [try.haxe.org][2].

## References

 - ["Do-while", Haxe manual][1]


  [1]: https://haxe.org/manual/expression-do-while.html
  [2]: http://try.haxe.org/#0B66A

## Flow control
The flow or execution of a loop can be controlled by use of `break` and `continue` expressions.

## Break

`break` exits the current loop. In case the loop is nested inside another loop, the parent loop is unaffected.

    for (i in 0...10) {
        for (j in 0...10) {
            if (j == 5) break;
            trace(i, j);
        }
    }

Try the example on [try.haxe.org][1].

## Continue

`continue` skips the current iteration of the loop at the point of the expression. In case the loop is nested inside another loop, the parent loop is unaffected.

    for (i in 0...10) {
        for (j in 0...10) {
            if (j == 5) continue;
            trace(i, j);
        }
    }

Try the example on [try.haxe.org][2].

## References

 - ["Break", Haxe manual][3]
 - ["Continue", Haxe manual][4]


  [1]: http://try.haxe.org/#eBD2C
  [2]: http://try.haxe.org/#19450
  [3]: https://haxe.org/manual/expression-break.html
  [4]: https://haxe.org/manual/expression-continue.html

