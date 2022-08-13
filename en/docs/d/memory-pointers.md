---
title: "Memory & Pointers"
slug: "memory--pointers"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
- &<variable\> - access by reference (=gets the pointer to the data of the variable)
- \*<variable\> - deference operator (=gets the data object from a pointer)
- <type\>* - data type that points to <type\> (e.g. `int*) 

## Pointers
D is a system programming language and thus allows you to manually manage and mess up your memory. Nevertheless, D uses a garbage collector per default to free unused memory.

D provides pointer types T* like in C:

<!-- language: lang-d -->


    void main()
    {
        int a;
        int* b = &a; // b contains address of a
        auto c = &a; // c is int* and contains address of a

        import std.stdio : writeln;
        writeln("a ", a);
        writeln("b ", b);
        writeln("c ", c);
    }


## Allocating on the heap
A new memory block on the heap is allocated using the `new` expression, which returns a pointer to the managed memory:

<!-- language: lang-d -->

    void main()
    {
        int* a = new int;
        *a = 42; // dereferencing
        import std.stdio : writeln;
        writeln("a: ", *a);
    }

## @safe D
As soon as the memory referenced by a isn't referenced anymore through any variable in the program, the garbage collector will free its memory.

D also allows pointer arithmetic, except in code that is marked as `@safe`.

<!-- language: lang-d -->

    void safeFun() @safe
    {
        writeln("Hello World");
        // allocating memory with the GC is safe too
        int* p = new int;
    }
    
    void unsafeFun()
    {
        int* p = new int;
        int* fiddling = p + 5;
    }
    
    void main()
    {
        safeFun();
        unsafeFun();
    }

For more information about SafeD see the [article](https://dlang.org/safed.html) from the D design team.

