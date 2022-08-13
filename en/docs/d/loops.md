---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
- for (<initializer\>; <loop condition\>; <loop statement\>) { <statements\> }
- while (<condition\>) { <statements\> }
- do { <statements\> } while (<condition\>);
- foreach (<el\>, <collection\>)
- foreach_reverse (<el\>, <collection\>)

- `for` loop in [_Programming in D_](http://ddili.org/ders/d.en/for.html), [specification](https://dlang.org/spec/statement.html#ForStatement)
- `while` loop in [_Programming in D_](http://ddili.org/ders/d.en/while.html), [specification](https://dlang.org/spec/statement.html#WhileStatement)
- `do while` loop in [_Programming in D_](http://ddili.org/ders/d.en/do_while.html), [specification](https://dlang.org/spec/statement.html#do-statement)
- `foreach` in [_Programming in D_](http://ddili.org/ders/d.en/foreach.html), [opApply](http://ddili.org/ders/d.en/foreach_opapply.html), [specification](https://dlang.org/spec/statement.html#ForeachStatement)

You can play with [loops](http://tour.dlang.io/tour/en/basics/loops) and [foreach](http://tour.dlang.io/tour/en/basics/foreach) online.

## For loop
<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        for (int i = 0; i < arr.length; i++)
        {
            arr[i] *= 2;
        }
        writeln(arr); // [2, 6, 8]
    }

## While loop
<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        int i = 0;
        while (i < arr.length)
        {
            arr[i++] *= 2;
        }
        writeln(arr); // [2, 6, 8]
    }

## do-while
<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        int i = 0;
        assert(arr.length > 0, "Array must contain at least one element");
        do
        {
            arr[i++] *= 2;
        } while (i < arr.length);
        writeln(arr); // [2, 6, 8]
    }

## Foreach
Foreach allows a less error-prone and better readable way to iterate collections. The attribute `ref` can be used if we want to directly modify the iterated element.

<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        foreach (ref el; arr)
        {
            el *= 2;
        }
        writeln(arr); // [2, 6, 8]
    }

The index of the iteration can be accessed too:

<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        foreach (i, el; arr)
        {
            arr[i] = el * 2;
        }
        writeln(arr); // [2, 6, 8]
    }

Iteration in reverse order is possible too:

<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        int i = 0;
        foreach_reverse (ref el; arr)
        {
            el += i++; // 4 is incremented by 0, 3 by 1, and 1 by 2
        }
        writeln(arr); // [3, 4, 4]
    }

## Break, continue & labels
<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4, 5];
        foreach (i, el; arr)
        {
            if (i == 0)
                continue; // continue with the next iteration
            arr[i] *= 2;
            if (i == 2)
                break; // stop the loop iteration
        }
        writeln(arr); // [1, 6, 8, 5]
    }

Labels can also be used to `break` or `continue` within nested loops.

<!-- language: lang-d -->

    void main()
    {
        import std.stdio : writeln;
        int[] arr = [1, 3, 4];
        outer: foreach (j; 0..10) // iterates with j=0 and j=1
            foreach (i, el; arr)
            {
                arr[i] *= 2;
                if (j == 1)
                    break outer; // stop the loop iteration
        }
        writeln(arr); // [4, 6, 8] (only 1 reaches the second iteration)
    }

