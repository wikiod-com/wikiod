---
title: "Types"
slug: "types"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Introduction to Types
Types can represents various kind of things. It can be a single data, a set of data or a function.

In F#, we can group the types into two categories.:

 - F# types:

        // Functions
        let a = fun c -> c
    
        // Tuples
        let b = (0, "Foo")
    
        // Unit type
        let c = ignore
        
        // Records
        type r = { Name : string; Age : int }
        let d = { Name = "Foo"; Age = 10 }
        
        // Discriminated Unions
        type du = | Foo | Bar
        let e = Bar
        
        // List and seq
        let f = [ 0..10 ]
        let g = seq { 0..10 }

        // Aliases
        type MyAlias = string

 - .NET types

     - Built-in type (int, bool, string,...)
     - Classes, Structs & Interfaces
     - Delegates
     - Arrays

## Type Abbreviations 
Type abbreviations allow you to create aliases on existing types to give them a more meaningful senses.

    // Name is an alias for a string
    type Name = string
    
    // PhoneNumber is an alias for a string
    type PhoneNumber = string

Then you can use the alias just as any other type:

    // Create a record type with the alias
    type Contact = {
        Name : Name
        Phone : PhoneNumber }

    // Create a record instance
    // We can assign a string since Name and PhoneNumber are just aliases on string type
    let c = {
        Name = "Foo"
        Phone = "00 000 000" }

    printfn "%A" c

    // Output
    // {Name = "Foo";
    // Phone = "00 000 000";}

Be careful, aliases does not check for type consistency. This means that two aliases that target the same type can be assigned to each other:

    let c = {
        Name = "Foo"
        Phone = "00 000 000" }
    let d = {
        Name = c.Phone
        Phone = c.Name }

    printfn "%A" d

    // Output
    // {Name = "00 000 000";
    // Phone = "Foo";}

## Types are created in F# using type keyword
`F#` uses the `type` keyword to create different kind of types. 

  1. Type aliases
  2. Discriminated union types
  3. Record types
  4. Interface types
  5. Class types
  6. Struct types

Examples with equivalent `C#` code where possible:

    // Equivalent C#:
    //  using IntAliasType = System.Int32;
    type IntAliasType = int // As in C# this doesn't create a new type, merely an alias

    type DiscriminatedUnionType =
      | FirstCase
      | SecondCase  of int*string

      member x.SomeProperty = // We can add members to DU:s
        match x with
        | FirstCase         -> 0
        | SecondCase (i, _) -> i

    type RecordType =
      {
        Id    : int
        Name  : string
      }
      static member New id name : RecordType = // We can add members to records
        { Id = id; Name = name } // { ... } syntax used to create records

    // Equivalent C#:
    //  interface InterfaceType
    //  {
    //    int     Id    { get; }
    //    string  Name  { get; }
    //    int Increment (int i);
    //  }  
    type InterfaceType =
      interface // In order to create an interface type, can also use [<Interface>] attribute
        abstract member Id        : int
        abstract member Name      : string
        abstract member Increment : int -> int
      end

    // Equivalent C#:
    //  class ClassType : InterfaceType
    //  {
    //    static int increment (int i)
    //    {
    //      return i + 1;
    //    }
    //  
    //    public ClassType (int id, string name)
    //    {
    //      Id    = id    ;
    //      Name  = name  ;
    //    }
    //  
    //    public int    Id    { get; private set; }
    //    public string Name  { get; private set; }
    //    public int   Increment (int i)
    //    {
    //      return increment (i);
    //    }
    //  }
    type ClassType (id : int, name : string) = // a class type requires a primary constructor
      let increment i = i + 1 // Private helper functions

      interface InterfaceType with // Implements InterfaceType
        member x.Id           = id
        member x.Name         = name
        member x.Increment i  = increment i


    // Equivalent C#:
    //  class SubClassType : ClassType
    //  {
    //    public SubClassType (int id, string name) : base(id, name)
    //    {
    //    }
    //  }
    type SubClassType (id : int, name : string) =
      inherit ClassType (id, name) // Inherits ClassType

    // Equivalent C#:
    //  struct StructType
    //  {
    //    public StructType (int id)
    //    {
    //      Id = id;
    //    }
    //  
    //    public int Id { get; private set; }
    //  }
    type StructType (id : int) =
      struct  // In order create a struct type, can also use [<Struct>] attribute
        member x.Id = id
      end


## Type Inference
**Acknowledgement**

This example is adapted from this article on [type inference][1] 

**What is type Inference?**

Type Inference is the mechanism that allows the compiler to deduce what types are used and where. This mechanism is based on an algorithm often called “Hindley-Milner” or “HM”. See below some of the rules for determine the types of simple and function values:

 - Look at the literals
 - Look at the functions and other values something interacts with
 - Look at any explicit type constraints
 - If there are no constraints anywhere, automatically generalize to generic types
 
**Look at the literals**

The compiler can deduce types by looking at the literals. If the literal is an int and you are adding “x” to it, then “x” must be an int as well. But if the literal is a float and you are adding “x” to it, then “x” must be a float as well.

Here are some examples:

    let inferInt x = x + 1
    let inferFloat x = x + 1.0
    let inferDecimal x = x + 1m     // m suffix means decimal
    let inferSByte x = x + 1y       // y suffix means signed byte
    let inferChar x = x + 'a'       // a char
    let inferString x = x + "my string"

**Look at the functions and other values it interacts with**

If there are no literals anywhere, the compiler tries to work out the types by analysing the functions and other values that they interact with.

    let inferInt x = x + 1
    let inferIndirectInt x = inferInt x       //deduce that x is an int
    
    let inferFloat x = x + 1.0
    let inferIndirectFloat x = inferFloat x   //deduce that x is a float
    
    let x = 1
    let y = x     //deduce that y is also an int

**Look at any explicit type constraints or annotations**

If there are any explicit type constraints or annotations specified, then the compiler will use them.

    let inferInt2 (x:int) = x                // Take int as parameter
    let inferIndirectInt2 x = inferInt2 x    // Deduce from previous that x is int
    
    let inferFloat2 (x:float) = x                // Take float as parameter
    let inferIndirectFloat2 x = inferFloat2 x    // Deduce from previous that x is float

**Automatic generalization**

If after all this, there are no constraints found, the compiler just makes the types generic.

    let inferGeneric x = x 
    let inferIndirectGeneric x = inferGeneric x 
    let inferIndirectGenericAgain x = (inferIndirectGeneric x).ToString() 

**Things that can go wrong with type inference**

The type inference isn’t perfect, alas. Sometimes the compiler just doesn’t have a clue what to do. Again, understanding what is happening will really help you stay calm instead of wanting to kill the compiler. Here are some of the main reasons for type errors:

 - Declarations out of order
 - Not enough information
 - Overloaded methods

**Declarations out of order**

A basic rule is that you must declare functions before they are used.

This code fails:

    let square2 x = square x   // fails: square not defined
    let square x = x * x

But this is ok:

    let square x = x * x       
    let square2 x = square x   // square already defined earlier

**Recursive or simultaneous declarations**

A variant of the “out of order” problem occurs with recursive functions or definitions that have to refer to each other. No amount of reordering will help in this case – we need to use additional keywords to help the compiler.

When a function is being compiled, the function identifier is not available to the body. So if you define a simple recursive function, you will get a compiler error. The fix is to add the “rec” keyword as part of the function definition. For example:

    // the compiler does not know what "fib" means
    let fib n =
       if n <= 2 then 1
       else fib (n - 1) + fib (n - 2)
       // error FS0039: The value or constructor 'fib' is not defined

Here’s the fixed version with “rec fib” added to indicate it is recursive:

    let rec fib n =              // LET REC rather than LET
       if n <= 2 then 1
       else fib (n - 1) + fib (n - 2)

**Not enough information**

Sometimes, the compiler just doesn’t have enough information to determine a type. In the following example, the compiler doesn’t know what type the Length method is supposed to work on. But it can’t make it generic either, so it complains.

    let stringLength s = s.Length
      // error FS0072: Lookup on object of indeterminate type
      // based on information prior to this program point.
      // A type annotation may be needed ...

These kinds of error can be fixed with explicit annotations.

    let stringLength (s:string) = s.Length

**Overloaded methods**

When calling an external class or method in .NET, you will often get errors due to overloading.

In many cases, such as the concat example below, you will have to explicitly annotate the parameters of the external function so that the compiler knows which overloaded method to call.

    let concat x = System.String.Concat(x)           //fails
    let concat (x:string) = System.String.Concat(x)  //works
    let concat x = System.String.Concat(x:string)    //works

Sometimes the overloaded methods have different argument names, in which case you can also give the compiler a clue by naming the arguments. Here is an example for the StreamReader constructor.

    let makeStreamReader x = new System.IO.StreamReader(x)        //fails
    let makeStreamReader x = new System.IO.StreamReader(path=x)   //works


  [1]: https://fsharpforfunandprofit.com/posts/type-inference/

