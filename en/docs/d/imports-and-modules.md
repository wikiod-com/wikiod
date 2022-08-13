---
title: "Imports and modules"
slug: "imports-and-modules"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
- module my.package;
- import my.package;
- import my.package : function;
- import fancyName = mypackage;
- import my.package : fancyFunctionName = function;

Modules automatically provide a namespace scope for their contents. Modules superficially resemble classes, but differ in that:

- There's only one instance of each module, and it is statically allocated.
- There is no virtual table.
- Modules do not inherit, they have no super modules, etc.
- Only one module per file.
- Module symbols can be imported.
- Modules are always compiled at global scope, and are unaffected by surrounding attributes or other modifiers.
- Modules can be grouped together in hierarchies called packages.

Modules offer several guarantees:

- The order in which modules are imported does not affect the semantics.
- The semantics of a module are not affected by what imports it.
- If a module C imports modules A and B, any modifications to B will not silently change code in C that is dependent on A.

## Global imports
<!-- language: lang-d --> 

    import std.stdio;
    void main()
    {
        writeln("Hello World!");
    }

Multiple imports can either be specified in the same line, separated with a `comma` or in a new line.

<!-- language: lang-d --> 

    import std.stdio, std.math;
    import std.datetime;
    void main()
    {
        writeln("2^4: ", pow(2, 4));
        writeln("Current time: ", Clock.currTime());
    }

## Selective imports
Selective imports can help to cleanup the namespace and speed-up the compile-time even more, because the compiler only needs to parse the specific, selected functions.

<!-- language: lang-d --> 

    import std.stdio: writeln;
    void main()
    {
        writeln("Hello world");
    }

## Local imports
You can also import symbols in any scope, the import will only be looked up when the scope is needed (i.e. compiled) and the imported names will only be exposed in the imported scope. Most commonly the scope for local imports are functions, structs and classes.

<!-- language: lang-d --> 

    void main()
    {
        import std.stdio: writeln;
        writeln("Hello world");
    }
    // writeln isn't defined here

## Public imports
Modules can be exposed to other modules with `public imports`.

<!-- language: lang-d --> 

    public import std.math;
    // only exports the symbol 'pow'
    public import std.math : pow;

## Renamed imports
A local name for an import can be given, through which all references to the module's symbols must be qualified with:

<!-- language: lang-d --> 

    import io = std.stdio; 
    void main()
    {
        io.writeln("Hello world");
        std.stdio.writeln("hello!"); // error, std is undefined
        writeln("hello!");           // error, writeln is undefined
    }

Renamed imports are handy when dealing with very long import names.

## Renamed and selective imports
Selective imports may also be renamed.

<!-- language: lang-d --> 

    void main()
    {
        import std.stdio : fooln = writeln;
        fooln("Hello world");
    }

## Module declaration
Modules have a one-to-one correspondence with source files. The module name is, by default, the file name with the path and extension stripped off, and can be set explicitly with the module declaration.
The `ModuleDeclaration` sets the name of the module and what package it belongs to. If absent, the module name is taken to be the same name (stripped of path and extension) of the source file name.

<!-- language: lang-d --> 

    module my.fancy.module;

