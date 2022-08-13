---
title: "Compilador JIT"
slug: "compilador-jit"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

A compilação JIT, ou compilação just-in-time, é uma abordagem alternativa para interpretação de código ou compilação antecipada. A compilação JIT é usada na estrutura .NET. O código CLR (C#, F#, Visual Basic, etc.) é compilado primeiro em algo chamado Interpreted Language, ou IL. Este é um código de nível inferior que está mais próximo do código de máquina, mas não é específico da plataforma. Em vez disso, em tempo de execução, esse código é compilado em código de máquina para o sistema relevante.

Por que usar a compilação JIT?
- Melhor compatibilidade: cada linguagem CLR precisa apenas de um compilador para IL, e este IL pode ser executado em qualquer plataforma na qual possa ser convertido em código de máquina.
- Velocidade: a compilação JIT tenta combinar a velocidade de execução do código compilado antecipadamente e a flexibilidade de interpretação (pode analisar o código que será executado para possíveis otimizações antes da compilação)

Página da Wikipedia para obter mais informações sobre a compilação JIT em geral: https://en.wikipedia.org/wiki/Just-in-time_compilation

## Exemplo de compilação de IL
Aplicação simples do Hello World:

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

Código IL equivalente (que será compilado JIT)
    
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
        
Gerado com a ferramenta MS ILDASM (disassembler IL)

