---
title: ".NET Framework'ü kullanmaya başlama"
slug: "net-frameworku-kullanmaya-baslama"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## C# ile Merhaba Dünya
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


"Console.WriteLine" birkaç aşırı yüke sahip. Bu durumda, "Merhaba Dünya" dizesi parametredir ve yürütme sırasında "Merhaba Dünya"yı standart çıkış akışına çıkaracaktır. Diğer aşırı yüklemeler, akışa yazmadan önce argümanın ".ToString" öğesini çağırabilir. Daha fazla bilgi için [.NET Framework Belgeleri][1]'ne bakın.

[.NET Fiddle'da Canlı Demo İş Başında](https://dotnetfiddle.net/S7hjxp)

[C#'a Giriş](https://www.wikiod.com/tr/docs/c%23/15/compile-and-run-your-first-c-sharp-program)


[1]: https://msdn.microsoft.com/en-us/library/system.console.writeline

## F#'da Merhaba Dünya
    open System
    
    [<EntryPoint>]
    let main argv = 
        printfn "Hello World" 
        0 

[.NET Fiddle'da Canlı Demo İş Başında](https://dotnetfiddle.net/hDvqwC)

[F#'a Giriş](https://www.wikiod.com/tr/docs/f%23/817/introduction-to-f)

## Visual Basic .NET'te Merhaba Dünya
<!-- dil: vb.net -->

    Imports System

    Module Program
        Public Sub Main()
            Console.WriteLine("Hello World")
        End Sub
    End Module

[.NET Fiddle'da Canlı Demo İş Başında](https://dotnetfiddle.net/dRDZVe)

[Visual Basic .NET'e Giriş](https://www.wikiod.com/tr/vb-dotnet/visual-basic-net-dilini-kullanmaya-baslama )

## C++/CLI'de Merhaba Dünya
    using namespace System;
    
    int main(array<String^>^ args)
    {
        Console::WriteLine("Hello World");
    }


## IL'de Merhaba Dünya
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




## PowerShell'de Merhaba Dünya
    Write-Host "Hello World"

[PowerShell'e Giriş](https://www.wikiod.com/tr/powershell/powershelli-kullanmaya-baslama)

## Nemerle'de Merhaba Dünya
    System.Console.WriteLine("Hello World");

## Oksijende Merhaba Dünya
<!-- dil: lang-pascal -->

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

## Boo'da Merhaba Dünya
    print "Hello World"

## Python'da Merhaba Dünya (IronPython)
    print "Hello World"

<!---->

    import clr
    from System import Console
    Console.WriteLine("Hello World")

