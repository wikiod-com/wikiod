---
title: "Difference between Python and IronPython"
slug: "difference-between-python-and-ironpython"
draft: false
images: []
weight: 9881
type: docs
toc: true
---

## Using .Net assemblies from Python code
With IronPython you can access any .net assembly which is compiled using the same or a lower version than the IronPython core.

Example: Importing a a .net assembly and class

`from System import Math`

Example: Using an imported class:

    from System import Math
    print Math.Abs(-123)

You can also load additional assemblies by using the builtin `clr` module.

    import clr
    clr.AddReference('Sample') # Sample.dll inside of the working directory.

Than just use it as any other .net or python library.

## IronPython is written in pure c#
IronPython is completly written using managed .net (c#) code. So all `builtin` python methods and libraries (such as `next()`, `int()`, etc.) are writtin in .net.

This example shows the implementation of `len()` for a list of different types (only a few):

    ....

    public static int len([NotNull]List/*!*/ list) {
        return list.__len__();
    }

    public static int len([NotNull]PythonTuple/*!*/ tuple) {
        return tuple.__len__();
    }

    public static int len([NotNull]PythonDictionary/*!*/ dict) {
        return dict.__len__();
    }

   ....

If we would need some other type to count the length off, just add them in `Builtin.cs` and it will be available automatically.

## Using generics within IronPython
IronPython enables to use generic classes and methods from the .net framework. 
Generics can be used with the same syntax as accessing an index. For passing more than one type-parameter, they must be separated with a comma:

    l = Dictionary[int, str]()

That way we create a dictionary where keys only accepts `integers` and the values must be a `string`.

A sample usage could look like this

    from System.Collections.Generic import List
    lst = List[str]()
    lst.Add('Hello')
    lst.Add('World')
    for l in lst:
        print

Output
> Hello 
>
> World

When adding new items, type checking will also be performed:

    lst = List[str]()
    lst.Add(123)

>Traceback (most recent call last):
>
>File "\<stdin\>", line 1, in <module>
>
>TypeError: expected str, got int

