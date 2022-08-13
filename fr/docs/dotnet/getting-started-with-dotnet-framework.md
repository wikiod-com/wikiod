---
title: "Premiers pas avec .NET Framework"
slug: "premiers-pas-avec-net-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Bonjour tout le monde en C#
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


`Console.WriteLine` a plusieurs surcharges. Dans ce cas, la chaîne "Hello World" est le paramètre, et il sortira le "Hello World" dans le flux de sortie standard pendant l'exécution. D'autres surcharges peuvent appeler le `.ToString` de l'argument avant d'écrire dans le flux. Consultez la [Documentation .NET Framework][1] pour plus d'informations.

[Démo en direct en action sur .NET Fiddle] (https://dotnetfiddle.net/S7hjxp)

[Introduction à C#](https://www.wikiod.com/fr/docs/c%23/15/compile-and-run-your-first-c-sharp-program)


[1] : https://msdn.microsoft.com/en-us/library/system.console.writeline

## Bonjour le monde en F#
    open System
    
    [<EntryPoint>]
    let main argv = 
        printfn "Hello World" 
        0 

[Démo en direct en action sur .NET Fiddle] (https://dotnetfiddle.net/hDvqwC)

[Introduction à F#](https://www.wikiod.com/fr/docs/f%23/817/introduction-to-f)

## Bonjour tout le monde dans Visual Basic .NET
<!-- langue : vb.net -->

    Imports System

    Module Program
        Public Sub Main()
            Console.WriteLine("Hello World")
        End Sub
    End Module

[Démo en direct en action sur .NET Fiddle] (https://dotnetfiddle.net/dRDZVe)

[Introduction à Visual Basic .NET](https://www.wikiod.com/fr/vb-dotnet/premiers-pas-avec-le-langage-visual-basic-net )

## Bonjour tout le monde en C++/CLI
    using namespace System;
    
    int main(array<String^>^ args)
    {
        Console::WriteLine("Hello World");
    }


## Bonjour tout le monde en Illinois
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




## Bonjour tout le monde dans PowerShell
    Write-Host "Hello World"

[Introduction à PowerShell](https://www.wikiod.com/fr/powershell/premiers-pas-avec-powershell)

## Hello World à Nemerle
    System.Console.WriteLine("Hello World");

## Bonjour tout le monde dans Oxygene
<!-- langue : lang-pascal -->

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

## Bonjour tout le monde dans Boo
    print "Hello World"

## Bonjour tout le monde en Python (IronPython)
    print "Hello World"

<!---->

    import clr
    from System import Console
    Console.WriteLine("Hello World")

