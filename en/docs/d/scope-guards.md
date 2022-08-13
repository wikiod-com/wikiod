---
title: "Scope guards"
slug: "scope-guards"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
- scope(exit) - statements are called no matter how the current block was exited
- scope(success) - statements are called when the current block was exited normally
- scope(failure) -  statements are called when the current block was exited through exception throwing

Using scope guards makes code much cleaner and allows to place resource allocation and clean up code next to each other. These little helpers also improve safety because they make sure certain cleanup code is always called independent of which paths are actually taken at runtime.

The D scope feature effectively replaces the RAII idiom used in C++ which often leads to special scope guards objects for special resources.

Scope guards are called in the reverse order they are defined.

[Play with scope guards](http://tour.dlang.io/tour/en/gems/scope-guards) or [see an extensive tutorial](http://ddili.org/ders/d.en/scope.html).

## Place allocation and cleanup code next to each other
Scope guards allow executing statements at certain conditions if the current block is left.

<!-- language: lang-d -->

    import core.stdc.stdlib;

    void main() {
        int* p = cast(int*)malloc(int.sizeof);
        scope(exit) free(p);
    }

## Multiple, nested scopes
<!-- language: lang-d -->

    import std.stdio;

    void main() {
        writeln("<html>");
        scope(exit) writeln("</html>");
        {
            writeln("\t<head>");
            scope(exit) writeln("\t</head>");
            "\t\t<title>%s</title>".writefln("Hello");
        } // the scope(exit) on the previous line is executed here
    
        writeln("\t<body>");
        scope(exit) writeln("\t</body>");
    
        writeln("\t\t<h1>Hello World!</h1>");
    }

prints

<!-- language: lang-html -->

    <html>
        <head>
            <title>Hello</title>
        </head>
        <body>
            <h1>Hello World!</h1>
        </body>
    </html>

