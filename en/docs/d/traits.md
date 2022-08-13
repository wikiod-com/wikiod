---
title: "Traits"
slug: "traits"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - __traits (TraitsKeyword, TraitsArguments...)

## Iterating over the members of a struct
    import std.stdio;
    
    struct A {
        int b;
        void c();
        string d;
    };
    
    void main() {
        // The following foreach is unrolled in compile time
        foreach(name; __traits(allMembers, A)) {
            pragma(msg, name);
        }
    }

The `allMembers` traits returns a tuple of string containing the names of the members of the given type. These strings are known at compile time.

## Iterating over members of a struct/class without their inherited members
    module main;
    
    auto getMemberNames(T)() @safe pure {
        string[] members;
        
        foreach (derived; __traits(derivedMembers, T)) {
            members ~= derived;
        }
        
        return members;
    }
    
    class Foo {
        int a;
        int b;
    }
    
    class Bar : Foo {
        int c;
        int d;
        int e;
    }
    
    void main() {
        import std.stdio;
        
        foreach (member; getMemberNames!Bar) {
            writeln(member);
        }
    }

*derivedMembers* returns a tuple of string literals, where each string is the member name.

The example outputs:

    c
    d
    e

