---
title: "Analyze source code with Roslyn"
slug: "analyze-source-code-with-roslyn"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Analyze a simple "Hello World" application in C#
Create a new console application with one line in the `Main` method: `Console.WriteLine("Hello World")`

Remember the path to the `.csproj` file and replace it in the example.

Create a new **Console Application** and install the `Microsoft.CodeAnalysis` NuGet package and try the following code:
<!-- language: lang-c# -->
    const string projectPath = @"C:\HelloWorldApplication\HelloWorldProject.csproj";

    // Creating a build workspace.
    var workspace = MSBuildWorkspace.Create();
            
    // Opening the Hello World project.
    var project = workspace.OpenProjectAsync(projectPath).Result;

    // Getting the compilation.
    var compilation = project.GetCompilationAsync().Result;

    foreach (var tree in compilation.SyntaxTrees)
    {
        Console.WriteLine(tree.FilePath);

        var rootSyntaxNode = tree.GetRootAsync().Result;

        foreach (var node in rootSyntaxNode.DescendantNodes())
        {
            Console.WriteLine($" *** {node.Kind()}");
            Console.WriteLine($"     {node}");
        }
    }

    Console.ReadKey();

This will print all the files and all the **syntax nodes** in your **Hello World** project.

## Parse source code from text in C#
<!-- language: lang-c# -->
    var syntaxTree = CSharpSyntaxTree.ParseText(
    @"using System;
    using System.Collections;
    using System.Linq;
    using System.Text;

    namespace HelloWorldApplication
    {
    class Program
    {
    static void Main(string[] args)
    {
    Console.WriteLine(""Hello World"");
    }
    }
    }");

    var root = syntaxTree.GetRoot() as CompilationUnitSyntax;

    var namespaceSyntax = root.Members.OfType<NamespaceDeclarationSyntax>().First();
            
    var programClassSyntax = namespaceSyntax.Members.OfType<ClassDeclarationSyntax>().First();

    var mainMethodSyntax = programClassSyntax.Members.OfType<MethodDeclarationSyntax>().First();

    Console.WriteLine(mainMethodSyntax.ToString());

    Console.ReadKey();

This example will print the `Main` method from the text analyzing the syntax.

## Introspective analysis of an analyzer in C#
1. Create a new **Console Application**
2. Add the **NuGet** package `Microsoft.CodeAnalysis`
3. Import the namespaces `Microsoft.CodeAnalysis.MSBuild`, `System.Linq` and `Microsoft.CodeAnalysis.CSharp.Syntax`
4. Write the following example code in the `Main` method:

<!-- language: lang-c# -->
    // Declaring a variable with the current project file path.
    const string projectPath = @"C:\<your path to the project\<project file name>.csproj";
    
    // Creating a build workspace.
    var workspace = MSBuildWorkspace.Create();
            
    // Opening this project.
    var project = workspace.OpenProjectAsync(projectPath).Result;
    
    // Getting the compilation.
    var compilation = project.GetCompilationAsync().Result;
    
    // As this is a simple single file program, the first syntax tree will be the current file.
    var syntaxTree = compilation.SyntaxTrees.First();
    
    // Getting the root node of the file.
    var rootSyntaxNode = syntaxTree.GetRootAsync().Result;
    
    // Finding all the local variable declarations in this file and picking the first one.
    var firstLocalVariablesDeclaration = rootSyntaxNode.DescendantNodesAndSelf().OfType<LocalDeclarationStatementSyntax>().First();
    
    // Getting the first declared variable in the declaration syntax.
    var firstVariable = firstLocalVariablesDeclaration.Declaration.Variables.First();
    
    // Getting the text of the initialized value.
    var variableInitializer = firstVariable.Initializer.Value.GetFirstToken().ValueText;
    
    // This will print to screen the value assigned to the projectPath variable.
    Console.WriteLine(variableInitializer);

    Console.ReadKey();

When running the project, you will see the variable declared on top printed to the screen. This means that you successfully self analysed a project and found a variable in it.

## Analyze a simple "Hello World" application in VB.NET
Create a new console application with one line in the `Main` method: `Console.WriteLine("Hello World")`

Remember the path to the `.vbproj` file and replace it in the example.

Create a new **Console Application** and install the `Microsoft.CodeAnalysis` NuGet package and try the following code:
<!-- language: lang-vb -->
    Const projectPath = "C:\HelloWorldApplication\HelloWorldProject.vbproj"

    ' Creating a build workspace.
    Dim workspace = MSBuildWorkspace.Create()

    ' Opening the Hello World project.
    Dim project = workspace.OpenProjectAsync(projectPath).Result

    ' Getting the compilation.
    Dim compilation = project.GetCompilationAsync().Result

    For Each tree In compilation.SyntaxTrees

        Console.WriteLine(tree.FilePath)

        Dim rootSyntaxNode = tree.GetRootAsync().Result

        For Each node In rootSyntaxNode.DescendantNodes()

            Console.WriteLine($" *** {node.Kind()}")
            Console.WriteLine($"     {node}")
        Next
    Next

    Console.ReadKey()

This will print all the files and all the **syntax nodes** in your **Hello World** project.

## Get the type of 'var'
To get the actual type for a variable declared using `var`, call `GetSymbolInfo()` on the `SemanticModel`. You can open an existing solution using `MSBuildWorkspace`, then enumerate its projects and their documents. Use a document to obtain its `SyntaxRoot` and `SemanticModel`, then look for `VariableDeclarations` and retrieve the symbols for the `Type` of a declared variable like this:

<!-- language: lang-cs -->

    var workspace = MSBuildWorkspace.Create();
    var solution = workspace.OpenSolutionAsync("c:\\path\\to\\solution.sln").Result;

    foreach (var document in solution.Projects.SelectMany(project => project.Documents))
    {
        var rootNode = document.GetSyntaxRootAsync().Result;
        var semanticModel = document.GetSemanticModelAsync().Result;

        var variableDeclarations = rootNode
                .DescendantNodes()
                .OfType<LocalDeclarationStatementSyntax>();
        foreach (var varDeclaration in variableDeclarations)
        {
            var symbolInfo = semanticModel.GetSymbolInfo(varDeclaration.Declaration.Type);
            var typeSymbol = symbolInfo.Symbol; // the type symbol for the variable..
        }
    }

