---
title: "Runtime Compile"
slug: "runtime-compile"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## RoslynScript
`Microsoft.CodeAnalysis.CSharp.Scripting.CSharpScript` is a new C# script engine.

    var code = "(1 + 2).ToString()";
    var run = await CSharpScript.RunAsync(code, ScriptOptions.Default);
    var result = (string)run.ReturnValue;
    Console.WriteLine(result); //output 3

You can compile and run any statements, variables, methods, classes or any code segments.

## CSharpCodeProvider
`Microsoft.CSharp.CSharpCodeProvider` can be used to compile C# classes.

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

