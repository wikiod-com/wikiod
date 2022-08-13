---
title: "Compilation d'exécution"
slug: "compilation-dexecution"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## RoslynScript
`Microsoft.CodeAnalysis.CSharp.Scripting.CSharpScript` est un nouveau moteur de script C#.

    var code = "(1 + 2).ToString()";
    var run = await CSharpScript.RunAsync(code, ScriptOptions.Default);
    var result = (string)run.ReturnValue;
    Console.WriteLine(result); //output 3

Vous pouvez compiler et exécuter n'importe quelles instructions, variables, méthodes, classes ou segments de code.

## CSharpCodeProvider
`Microsoft.CSharp.CSharpCodeProvider` peut être utilisé pour compiler des classes C#.

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

