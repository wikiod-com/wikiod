---
title: "Dynamic Arrays & Slices"
slug: "dynamic-arrays--slices"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
- <type\>[] <name\>;

Slices generate a new view on existing memory. They don't create a new copy. If no slice holds a reference to that memory anymore - or a sliced part of it - it will be freed by the garbage collector.

Using slices it's possible to write very efficient code for e.g. parsers that just operate on one memory block and just slice the parts they really need to work on - no need allocating new memory blocks.

## Array operations
<!-- language: lang-d -->

    import std.stdio;

    void main() {
        int[] arr = [1, 2, 3];

        // concatenate
        arr ~= 4;
        writeln(arr); // [1, 2, 3, 4]

        // per element operations
        arr[] += 10
        writeln(arr); // [11, 12, 13, 14]
    }

## Declaration and initialization
<!-- language: lang-d -->

    import std.stdio;

    void main() {
        int[] arr = [1, 2, 3, 4];
    
        writeln(arr.length); // 4
        writeln(arr[2]); // 3
    
        // type inference still works
        auto arr2 = [1, 2, 3, 4];
        writeln(typeof(arr2).stringof); // int[]
    }

## Slices
<!-- language: lang-d -->
   
    import std.stdio;

    void main() {
        int[] arr = [1, 2, 3, 4, 5];
     
        auto arr2 = arr[1..$ - 1]; // .. is the slice syntax, $ represents the length of the array
        writeln(arr2); // [2, 3, 4]
    
        arr2[0] = 42;
        writeln(arr[1]); // 42
    }

