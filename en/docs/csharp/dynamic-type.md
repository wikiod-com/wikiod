---
title: "Dynamic type"
slug: "dynamic-type"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

The `dynamic` keyword declares a variable whose type is not known at compile time. A `dynamic` variable can contain any value, and the type of the value can change during runtime.

As noted in the book "Metaprogramming in .NET", C# does not have a backing type for the `dynamic` keyword:

> The functionality enabled by the `dynamic` keyword is a clever set of compiler actions that emit and use `CallSite` objects in the site container of the local execution scope. The compiler manages what programmers perceive as dynamic object
references through those `CallSite` instances.  The parameters, return types, fields, and properties that get dynamic treatment at compile time may be marked with some metadata to indicate that they were generated for dynamic use, but the underlying data type for them will always be `System.Object`.

 

## Creating a dynamic object with properties
    using System;
    using System.Dynamic;
    
    dynamic info = new ExpandoObject();
    info.Id = 123;
    info.Another = 456;
    
    Console.WriteLine(info.Another);
    // 456
    
    Console.WriteLine(info.DoesntExist);
    // Throws RuntimeBinderException

## Creating a dynamic variable
    dynamic foo = 123;
    Console.WriteLine(foo + 234);
    // 357    Console.WriteLine(foo.ToUpper())
    // RuntimeBinderException, since int doesn't have a ToUpper method

    foo = "123";
    Console.WriteLine(foo + 234);
    // 123234
    Console.WriteLine(foo.ToUpper()):
    // NOW A STRING

## Returning dynamic
    using System;

    public static void Main()
    {
        var value = GetValue();
        Console.WriteLine(value);
        // dynamics are useful!
    }
    
    private static dynamic GetValue()
    {
        return "dynamics are useful!";
    }

## Handling Specific Types Unknown at Compile Time


