---
title: "Compilação de tempo de execução"
slug: "compilacao-de-tempo-de-execucao"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## RoslynScript
`Microsoft.CodeAnalysis.CSharp.Scripting.CSharpScript` é um novo mecanismo de script C#.

    var code = "(1 + 2).ToString()";
    var run = await CSharpScript.RunAsync(code, ScriptOptions.Default);
    var result = (string)run.ReturnValue;
    Console.WriteLine(result); //output 3

Você pode compilar e executar qualquer instrução, variável, método, classe ou qualquer segmento de código.

## CSharpCodeProvider
`Microsoft.CSharp.CSharpCodeProvider` pode ser usado para compilar classes C#.

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

