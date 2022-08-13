---
title: "Primeros pasos con .NET Framework"
slug: "primeros-pasos-con-net-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hola Mundo en C#
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


`Console.WriteLine` tiene varias sobrecargas. En este caso, la cadena "Hello World" es el parámetro y generará "Hello World" en el flujo de salida estándar durante la ejecución. Otras sobrecargas pueden llamar a `.ToString` del argumento antes de escribir en la transmisión. Consulte la [Documentación de .NET Framework][1] para obtener más información.

[Demostración en vivo en acción en .NET Fiddle](https://dotnetfiddle.net/S7hjxp)

[Introducción a C#](https://www.wikiod.com/es/docs/c%23/15/compile-and-run-your-first-c-sharp-program)


[1]: https://msdn.microsoft.com/en-us/library/system.console.writeline

## Hola Mundo en F#
    open System
    
    [<EntryPoint>]
    let main argv = 
        printfn "Hello World" 
        0 

[Demostración en vivo en acción en .NET Fiddle](https://dotnetfiddle.net/hDvqwC)

[Introducción a F#](https://www.wikiod.com/es/docs/f%23/817/introduction-to-f)

## Hola Mundo en Visual Basic .NET
<!-- idioma: vb.net -->

    Imports System

    Module Program
        Public Sub Main()
            Console.WriteLine("Hello World")
        End Sub
    End Module

[Demostración en vivo en acción en .NET Fiddle](https://dotnetfiddle.net/dRDZVe)

[Introducción a Visual Basic .NET](https://www.wikiod.com/es/vb-dotnet/introduccion-al-lenguaje-visual-basic-net)

## Hola Mundo en C++/CLI
    using namespace System;
    
    int main(array<String^>^ args)
    {
        Console::WriteLine("Hello World");
    }


## Hola mundo en IL
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




## Hola Mundo en PowerShell
    Write-Host "Hello World"

[Introducción a PowerShell](https://www.wikiod.com/es/powershell/primeros-pasos-con-powershell)

## Hola Mundo en Nemerle
    System.Console.WriteLine("Hello World");

## Hola Mundo en Oxygene
<!-- idioma: lang-pascal -->

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

## Hola Mundo en Boo
    print "Hello World"

## Hola Mundo en Python (IronPython)
    print "Hello World"

<!---->

    import clr
    from System import Console
    Console.WriteLine("Hello World")

