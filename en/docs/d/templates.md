---
title: "Templates"
slug: "templates"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - template identifier (TemplateParameterList) { ... }
 - struct identifier (TemplateParameterList) { ... }
 - class identifier (TemplateParameterList) { ... }
 - ReturnType identifier (TemplateParameterList)(ParameterList) { ... }
 - identifier!(TemplateInvocationList)

## Function with one template
    import std.stdio;

    T min(T)(in T arg1, in T arg2) {
        return arg1 < arg2 ? arg1 : arg2;
    }

    void main() {
        //Automatic type inference
        writeln(min(1, 2));
        
        //Explicit type
        writeln(min!(ubyte)(1, 2));
        
        //With single type, the parenthesis might be ommited
        writeln(min!ubyte(1, 2));
    }

## template
An template can be introduced with ``template``. It can contain functions and classes and other constructs.

    template StaticArray(Type, size_t Length) {
         class StaticArray {
              Type content[Length];
              
              size_t myLength() {
                return getLength(this);
              }
         }
         
         private size_t getLength(StaticArray arr) {
              return Length;
         }
    }
    
    void main() {
        StaticArray!(int, 5) arr5 = new StaticArray!(int, 5);
        import std.stdio;
        writeln(arr5.myLength());
    }

