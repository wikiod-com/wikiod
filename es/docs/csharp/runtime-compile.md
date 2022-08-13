---
title: "Compilación en tiempo de ejecución"
slug: "compilacion-en-tiempo-de-ejecucion"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Guión de Roslyn
`Microsoft.CodeAnalysis.CSharp.Scripting.CSharpScript` es un nuevo motor de secuencias de comandos C#.

    var code = "(1 + 2).ToString()";
    var run = await CSharpScript.RunAsync(code, ScriptOptions.Default);
    var result = (string)run.ReturnValue;
    Console.WriteLine(result); //output 3

Puede compilar y ejecutar declaraciones, variables, métodos, clases o cualquier segmento de código.

## CSharpCodeProvider
`Microsoft.CSharp.CSharpCodeProvider` se puede usar para compilar clases de C#.

    var code = @"
        public class Abc {
           public string Get() { return ""abc""; }
        }
    ";

    var options = new CompilerParameters();
    options.GenerateExecutable = false;
    options.GenerateInMemory = false;

    var provider = new CSharpCodeProvider();
    var compile = provider.CompileAssemblyFromSource(options, code);

    var type = compile.CompiledAssembly.GetType("Abc");
    var abc = Activator.CreateInstance(type);

    var method = type.GetMethod("Get");
    var result = method.Invoke(abc, null);

    Console.WriteLine(result); //output: abc

