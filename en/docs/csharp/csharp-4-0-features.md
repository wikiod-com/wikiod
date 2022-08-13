---
title: "C# 4.0 Features"
slug: "c-40-features"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Optional parameters and named arguments
We can omit the argument in the call if that argument is an Optional Argument
Every Optional Argument has its own default value
It will take default value if we do not supply the value
A default value of a Optional Argument must be a
1. Constant expression.
2. Must be a value type such as enum or struct.
3. Must be an expression of the form default(valueType)

It must be set at the end of parameter list

Method parameters with default values:
 
    public void ExampleMethod(int required, string optValue = "test", int optNum = 42)
    {
        //...
    }

As said by MSDN, A named argument ,

Enables you to pass the argument to the function by associating the parameter’s name
No needs for remembering the parameters position that we are not aware of always.
No need to look the order of the parameters in the parameters list of called function.
We can specify parameter for each arguments by its name.

Named arguments:
    
    // required = 3, optValue = "test", optNum = 4
    ExampleMethod(3, optNum: 4);
    // required = 2, optValue = "foo", optNum = 42
    ExampleMethod(2, optValue: "foo");
    // required = 6, optValue = "bar", optNum = 1
    ExampleMethod(optNum: 1, optValue: "bar", required: 6);

**Limitation of using a Named Argument**

Named argument specification must appear after all fixed arguments have been specified.

If you use a named argument before a fixed argument you will get a compile time error as follows.

[![enter image description here][1]][1]

Named argument specification must appear after all fixed arguments have been specified


  [1]: http://i.stack.imgur.com/pzWLh.png

## Variance
Generic interfaces and delegates can have their type parameters marked as [_covariant_](https://www.wikiod.com/docs/c%23/27/generics/7362/covariance#t=201607241842437571339) or [_contravariant_](https://www.wikiod.com/docs/c%23/27/generics/7372/contravariance#t=201607241842437571339) using the `out` and `in` keywords respectively. These declarations are then respected for type conversions, both implicit and explicit, and both compile time and run time.

For example, the existing interface `IEnumerable<T>` has been redefined as being covariant:

    interface IEnumerable<out T>
    {
        IEnumerator<T> GetEnumerator();
    }

The existing interface IComparer<T> has been redefined as being contravariant:

    public interface IComparer<in T>
    {
        int Compare(T x, T y);
    }

## Dynamic member lookup
A new pseudo-type `dynamic` is introduced into the C# type system. It is treated as `System.Object`, but in addition, any member access (method call, field, property, or indexer access, or a delegate invocation) or application of an operator on a value of such type is permitted without any type checking, and its resolution is postponed until run-time. This is known as duck typing or late binding. For example:
 
    // Returns the value of Length property or field of any object
    int GetLength(dynamic obj)
    {
        return obj.Length;
    }
      
    GetLength("Hello, world");        // a string has a Length property,
    GetLength(new int[] { 1, 2, 3 }); // and so does an array,
    GetLength(42);                    // but not an integer - an exception will be thrown
                                      // in GetLength method at run-time

In this case, dynamic type is used to avoid more verbose Reflection. It still uses Reflection under the hood, but it's usually faster thanks to caching.

This feature is primarily targeted at interoperability with dynamic languages.

    // Initialize the engine and execute a file
    var runtime = ScriptRuntime.CreateFromConfiguration();
    dynamic globals = runtime.Globals;
    runtime.ExecuteFile("Calc.rb");
    
    // Use Calc type from Ruby
    dynamic calc = globals.Calc.@new();
    calc.valueA = 1337;
    calc.valueB = 666;
    dynamic answer = calc.Calculate();

Dynamic type has applications even in mostly statically typed code, for example it makes [double dispatch](https://en.wikipedia.org/wiki/Double_dispatch) posible without implementing Visitor pattern.

## Optional ref keyword when using COM
The ref keyword for callers of methods is now optional when calling into methods supplied by COM interfaces. Given a COM method with the signature

    void Increment(ref int x);
the invocation can now be written as either

    Increment(0); // no need for "ref" or a place holder variable any more

