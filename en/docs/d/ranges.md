---
title: "Ranges"
slug: "ranges"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

If a foreach is encountered by the compiler

```D
foreach (element; range) {
```

it's internally rewritten similar to the following:

```D
for (auto it = range; !it.empty; it.popFront()) {
    auto element = it.front;
    ...
}
```

Any object which fulfills the above interface is called an input range and is thus a type that can be iterated over:

```
struct InputRange {
    @property bool empty();
    @property T front();
    void popFront();
}
```

## Strings and arrays are ranges
```D
import std.stdio;

void main() {
    auto s = "hello world";
    auto a = [1, 2, 3, 4];

    foreach (c; s) {
        write(c, "!"); // h!e!l!l!o! !w!o!r!l!d!
    }
    writeln();

    foreach (x; a) {
        write(x * x, ", "); // 1, 4, 9, 16, 
    }
}
```

## Making a new Input Range type
The `InputRange` concept has three functions, example:
```D
struct InputRange(T) {
    @property bool empty();
    @property T front();
    void popFront();
}
```

In short, a way to

1. check if the range is empty
2. get the current element
3. move to the next element

To make our own type a `InputRange`, we must implement these three functions. Let's take a look at the infinite sequence of squares.
```D
struct SquaresRange {
    int cur = 1;

    @property bool empty() {
        return false;
    }

    @property int front() {
        return cur^^2;
    }

    void popFront() {
        cur++;
    }
}
```

See the [D tour](http://tour.dlang.io/tour/en/basics/ranges) for an example with Fibonacci.

