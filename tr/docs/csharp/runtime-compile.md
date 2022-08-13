---
title: "Çalışma Zamanı Derlemesi"
slug: "calsma-zaman-derlemesi"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## RoslynScript
`Microsoft.CodeAnalysis.CSharp.Scripting.CSharpScript` yeni bir C# komut dosyası motorudur.

    var code = "(1 + 2).ToString()";
    var run = await CSharpScript.RunAsync(code, ScriptOptions.Default);
    var result = (string)run.ReturnValue;
    Console.WriteLine(result); //output 3

Herhangi bir ifadeyi, değişkeni, yöntemi, sınıfı veya herhangi bir kod parçasını derleyebilir ve çalıştırabilirsiniz.

## CSharpCodeSağlayıcı
`Microsoft.CSharp.CSharpCodeProvider`, C# sınıflarını derlemek için kullanılabilir.

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

