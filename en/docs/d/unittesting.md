---
title: "Unittesting"
slug: "unittesting"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
- unittest { ... } - a block that is only run in "unittesting" mode
- assert(<expression that evaluates to a boolean\>, <optional error message\>) 

## Unittest blocks
Tests are an excellent way to ensure stable, bug-free applications. They serve as an interactive documentation and allow to modify code without fear to break functionality. D provides a convenient and native syntax for `unittest` block as part of the D language. Anywhere in a D module `unittest` blocks can be used to test functionality of the source code.


<!-- language: lang-d -->

    /**
    Yields the sign of a number.
    Params:
        n = number which should be used to check the sign
    Returns:
        1 for positive n, -1 for negative and 0 for 0.
    */
    T sgn(T)(T n)
    {
        if (n == 0)
            return 0;
        return (n > 0) ? 1 : -1;
    }

    // this block will only be executed with -unittest
    // it will be removed from the executable otherwise
    unittest
    {
        // go ahead and make assumptions about your function
        assert(sgn(10)  ==  1);
        assert(sgn(1)   ==  1);
        assert(sgn(-1)  == -1);
        assert(sgn(-10) == -1);
    }

## Executing unittest
If `-unittest` flag is passed to the D compiler, it will run all unittest blocks. Often it is useful to let the compiler generate a stubbed `main` function. Using the compile & run wrapper `rdmd`, testing your D program gets as easy as:

    rdmd -main -unittest yourcode.d

Of course you can also split this process into two steps if you want:

    dmd -main -unittest yourcode.d
    ./yourcode

For `dub` projects compiling all files and executing their unittest blocks can be done conveniently with

    dub test

<br>
Pro tip: define `tdmd` as shell alias to save tipping.

    alias tdmd="rdmd -main -unittest"

and then test your files with:

    tdmd yourcode.d

## Annotated unittest
For templated code it is often useful to verify that for function attributes (e.g. `@nogc` are inferred correctly. To ensure this for a specific test and thus type the entire unittest can be annotated

<!-- language: lang-d -->


    @safe @nogc pure nothrow unittest
    {
        import std.math;
        assert(exp(0)  ==  1);
        assert(log(1)  ==  0);
    }

Note that of course in D every block can be annotated with attributes and the compilers, of course, verifies that they are correct. So for example the following would be similar to the example above:

    unittest
    {
        import std.math;
        @safe {
            assert(exp(0)  ==  1);
            assert(log(1)  ==  0);
        }
    }

