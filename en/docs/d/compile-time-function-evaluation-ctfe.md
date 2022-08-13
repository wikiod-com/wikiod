---
title: "Compile Time Function Evaluation (CTFE)"
slug: "compile-time-function-evaluation-ctfe"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

CTFE is a mechanism which allows the compiler to execute functions at compile time. There is no special set of the D language necessary to use this feature - whenever a function just depends on compile time known values the D compiler might decide to interpret it during compilation.

You can also [play interactively](http://tour.dlang.io/tour/en/gems/compile-time-function-evaluation-ctfe) with CTFE.

## Evaluate a function at compile-time
<!-- language: lang-d -->

    long fib(long n)
    {
        return n < 2 ? n : fib(n - 1) + fib(n - 2);
    }

    struct FibStruct(int n) { // Remarks: n is a template
        ubyte[fib(n)] data;
    }

    void main()
    {
        import std.stdio : writeln;
        enum f10 = fib(10); // execute the function at compile-time
        pragma(msg, f10); // will print 55 during compile-time
        writeln(f10); // print 55 during runtime
        pragma(msg, FibStruct!11.sizeof); // The size of the struct is 89
    }

