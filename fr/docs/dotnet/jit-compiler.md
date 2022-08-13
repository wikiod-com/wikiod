---
title: "Compilateur JIT"
slug: "compilateur-jit"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

La compilation JIT, ou compilation juste-à-temps, est une approche alternative à l'interprétation du code ou à la compilation anticipée. La compilation JIT est utilisée dans le framework .NET. Le code CLR (C#, F#, Visual Basic, etc.) est d'abord compilé en quelque chose appelé Interpreted Language, ou IL. Il s'agit d'un code de niveau inférieur qui est plus proche du code machine, mais qui n'est pas spécifique à la plate-forme. Au lieu de cela, lors de l'exécution, ce code est compilé en code machine pour le système concerné.

Pourquoi utiliser la compilation JIT ?
- Meilleure compatibilité : chaque langage CLR n'a besoin que d'un seul compilateur pour IL, et cet IL peut s'exécuter sur n'importe quelle plate-forme sur laquelle il peut être converti en code machine.
- Vitesse : la compilation JIT tente de combiner la vitesse d'exécution du code compilé à l'avance et la flexibilité de l'interprétation (peut analyser le code qui sera exécuté pour des optimisations potentielles avant la compilation)

Page Wikipedia pour plus d'informations sur la compilation JIT en général : https://en.wikipedia.org/wiki/Just-in-time_compilation

## Exemple de compilation IL
Application Hello World simple :

    using System;
    
    namespace HelloWorld
    {
        class Program
        {
            static void Main(string[] args)
            {
                Console.WriteLine("Hello World");
            }
        }
    }

Code IL équivalent (qui sera compilé JIT)
    
    //  Microsoft (R) .NET Framework IL Disassembler.  Version 4.6.1055.0
    //  Copyright (c) Microsoft Corporation.  All rights reserved.
    
    
    
    // Metadata version: v4.0.30319
    .assembly extern mscorlib
    {
      .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
      .ver 4:0:0:0
    }
    .assembly HelloWorld
    {
      .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilationRelaxationsAttribute::.ctor(int32) = ( 01 00 08 00 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute::.ctor() = ( 01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
                                                                                                                 63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01 )       // ceptionThrows.
    
      // --- The following custom attribute is added automatically, do not uncomment -------
      //  .custom instance void [mscorlib]System.Diagnostics.DebuggableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggableAttribute/DebuggingModes) = ( 01 00 07 01 00 00 00 00 ) 
    
      .custom instance void [mscorlib]System.Reflection.AssemblyTitleAttribute::.ctor(string) = ( 01 00 0A 48 65 6C 6C 6F 57 6F 72 6C 64 00 00 )    // ...HelloWorld..
      .custom instance void [mscorlib]System.Reflection.AssemblyDescriptionAttribute::.ctor(string) = ( 01 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Reflection.AssemblyConfigurationAttribute::.ctor(string) = ( 01 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Reflection.AssemblyCompanyAttribute::.ctor(string) = ( 01 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Reflection.AssemblyProductAttribute::.ctor(string) = ( 01 00 0A 48 65 6C 6C 6F 57 6F 72 6C 64 00 00 )    // ...HelloWorld..
      .custom instance void [mscorlib]System.Reflection.AssemblyCopyrightAttribute::.ctor(string) = ( 01 00 12 43 6F 70 79 72 69 67 68 74 20 C2 A9 20   // ...Copyright .. 
                                                                                                      20 32 30 31 37 00 00 )                            //  2017..
      .custom instance void [mscorlib]System.Reflection.AssemblyTrademarkAttribute::.ctor(string) = ( 01 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.InteropServices.ComVisibleAttribute::.ctor(bool) = ( 01 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.InteropServices.GuidAttribute::.ctor(string) = ( 01 00 24 33 30 38 62 33 64 38 36 2D 34 31 37 32   // ..$308b3d86-4172
                                                                                                      2D 34 30 32 32 2D 61 66 63 63 2D 33 66 38 65 33   // -4022-afcc-3f8e3
                                                                                                      32 33 33 63 35 62 30 00 00 )                      // 233c5b0..
      .custom instance void [mscorlib]System.Reflection.AssemblyFileVersionAttribute::.ctor(string) = ( 01 00 07 31 2E 30 2E 30 2E 30 00 00 )             // ...1.0.0.0..
      .custom instance void [mscorlib]System.Runtime.Versioning.TargetFrameworkAttribute::.ctor(string) = ( 01 00 1C 2E 4E 45 54 46 72 61 6D 65 77 6F 72 6B   // ....NETFramework
                                                                                                            2C 56 65 72 73 69 6F 6E 3D 76 34 2E 35 2E 32 01   // ,Version=v4.5.2.
                                                                                                            00 54 0E 14 46 72 61 6D 65 77 6F 72 6B 44 69 73   // .T..FrameworkDis
                                                                                                            70 6C 61 79 4E 61 6D 65 14 2E 4E 45 54 20 46 72   // playName..NET Fr
                                                                                                            61 6D 65 77 6F 72 6B 20 34 2E 35 2E 32 )          // amework 4.5.2
      .hash algorithm 0x00008004
      .ver 1:0:0:0
    }
    .module HelloWorld.exe
    // MVID: {2A7E1D59-1272-4B47-85F6-D7E1ED057831}
    .imagebase 0x00400000
    .file alignment 0x00000200
    .stackreserve 0x00100000
    .subsystem 0x0003       // WINDOWS_CUI
    .corflags 0x00020003    //  ILONLY 32BITPREFERRED
    // Image base: 0x0000021C70230000
    
    
    // =============== CLASS MEMBERS DECLARATION ===================
    
    .class private auto ansi beforefieldinit HelloWorld.Program
           extends [mscorlib]System.Object
    {
      .method private hidebysig static void  Main(string[] args) cil managed
      {
        .entrypoint
        // Code size       13 (0xd)
        .maxstack  8
        IL_0000:  nop
        IL_0001:  ldstr      "Hello World"
        IL_0006:  call       void [mscorlib]System.Console::WriteLine(string)
        IL_000b:  nop
        IL_000c:  ret
      } // end of method Program::Main
    
      .method public hidebysig specialname rtspecialname 
              instance void  .ctor() cil managed
      {
        // Code size       8 (0x8)
        .maxstack  8
        IL_0000:  ldarg.0
        IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
        IL_0006:  nop
        IL_0007:  ret
      } // end of method Program::.ctor
    
    } // end of class HelloWorld.Program
        
Généré avec l'outil MS ILDASM (désassembleur IL)

