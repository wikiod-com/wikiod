---
title: "Introdução ao .NET Framework"
slug: "introducao-ao-net-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Olá Mundo em C#
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


`Console.WriteLine` tem várias sobrecargas. Neste caso, a string "Hello World" é o parâmetro, e irá gerar o "Hello World" para o fluxo de saída padrão durante a execução. Outras sobrecargas podem chamar o `.ToString` do argumento antes de gravar no fluxo. Consulte a [Documentação do .NET Framework][1] para obter mais informações.

[Demonstração ao vivo em ação no .NET Fiddle](https://dotnetfiddle.net/S7hjxp)

[Introdução ao C#](https://www.wikiod.com/pt/docs/c%23/15/compile-and-run-your-first-c-sharp-program)


[1]: https://msdn.microsoft.com/en-us/library/system.console.writeline

## Olá Mundo em F#
    open System
    
    [<EntryPoint>]
    let main argv = 
        printfn "Hello World" 
        0 

[Demonstração ao vivo em ação no .NET Fiddle](https://dotnetfiddle.net/hDvqwC)

[Introdução ao F#](https://www.wikiod.com/pt/docs/f%23/817/introduction-to-f)

## Olá Mundo em Visual Basic .NET
<!-- idioma: vb.net -->

    Imports System

    Module Program
        Public Sub Main()
            Console.WriteLine("Hello World")
        End Sub
    End Module

[Demonstração ao vivo em ação no .NET Fiddle](https://dotnetfiddle.net/dRDZVe)

[Introdução ao Visual Basic .NET](https://www.wikiod.com/pt/vb-dotnet/introducao-a-linguagem-visual-basic-net )

## Olá Mundo em C++/CLI
    using namespace System;
    
    int main(array<String^>^ args)
    {
        Console::WriteLine("Hello World");
    }


## Olá Mundo em IL
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




## Olá Mundo no PowerShell
    Write-Host "Hello World"

[Introdução ao PowerShell](https://www.wikiod.com/pt/powershell/introducao-ao-powershell)

## Olá Mundo em Nemerle
    System.Console.WriteLine("Hello World");

## Olá Mundo em Oxigênio
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

## Olá Mundo em Boo
    print "Hello World"

## Olá Mundo em Python (IronPython)
    print "Hello World"

<!---->

    import clr
    from System import Console
    Console.WriteLine("Hello World")

