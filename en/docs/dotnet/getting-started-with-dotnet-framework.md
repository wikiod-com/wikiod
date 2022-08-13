---
title: "Getting started with .NET Framework"
slug: "getting-started-with-net-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World in C#
    using System;
    
    class Program
    {
        // The Main() function is the first function to be executed in a program
        static void Main()
        {
            // Write the string "Hello World to the standard out
            Console.WriteLine("Hello World");
        }
    }


`Console.WriteLine` has several overloads. In this case, the string "Hello World" is the parameter, and it will output the "Hello World" to the standard out stream during execution. Other overloads may call the `.ToString` of the argument before writing to the stream.   See the [.NET Framework Documentation][1] for more information. 

[Live Demo in Action at .NET Fiddle](https://dotnetfiddle.net/S7hjxp)

[Introduction to C#](https://www.wikiod.com/docs/c%23/15/compile-and-run-your-first-c-sharp-program)


  [1]: https://msdn.microsoft.com/en-us/library/system.console.writeline

## Hello World in F#
    open System
    
    [<EntryPoint>]
    let main argv = 
        printfn "Hello World" 
        0 

[Live Demo in Action at .NET Fiddle](https://dotnetfiddle.net/hDvqwC)

[Introduction to F#](https://www.wikiod.com/docs/f%23/817/introduction-to-f)

## Hello World in Visual Basic .NET
<!-- language: vb.net -->

    Imports System

    Module Program
        Public Sub Main()
            Console.WriteLine("Hello World")
        End Sub
    End Module

[Live Demo in Action at .NET Fiddle](https://dotnetfiddle.net/dRDZVe)

[Introduction to Visual Basic .NET](https://www.wikiod.com/vb-dotnet/getting-started-with-visual-basic-net-language    )

## Hello World in C++/CLI
    using namespace System;
    
    int main(array<String^>^ args)
    {
        Console::WriteLine("Hello World");
    }


## Hello World in IL
    .class public auto ansi beforefieldinit Program
           extends [mscorlib]System.Object
    {
      .method public hidebysig static void  Main() cil managed
      { 
        .maxstack  8
        IL_0000:  nop
        IL_0001:  ldstr      "Hello World"
        IL_0006:  call       void [mscorlib]System.Console::WriteLine(string)
        IL_000b:  nop
        IL_000c:  ret
      }
    
      .method public hidebysig specialname rtspecialname 
              instance void  .ctor() cil managed
      {
        .maxstack  8
        IL_0000:  ldarg.0
        IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
        IL_0006:  ret
      }
    
    }




## Hello World in PowerShell
    Write-Host "Hello World"

[Introduction to PowerShell](https://www.wikiod.com/powershell/getting-started-with-powershell)

## Hello World in Nemerle
    System.Console.WriteLine("Hello World");

## Hello World in Oxygene
<!-- language: lang-pascal -->

    namespace HelloWorld;
    
    interface
    
    type
      App = class
      public
        class method Main(args: array of String);
      end;
    
    implementation
    
    class method App.Main(args: array of String);
    begin
      Console.WriteLine('Hello World');
    end;
    
    end.

## Hello World in Boo
    print "Hello World"

## Hello World in Python (IronPython)
    print "Hello World"

<!---->

    import clr
    from System import Console
    Console.WriteLine("Hello World")

