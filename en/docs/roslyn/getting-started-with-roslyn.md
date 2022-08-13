---
title: "Getting started with roslyn"
slug: "getting-started-with-roslyn"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
To start tinkering with Roslyn you will need the following NuGet packages:

 - The C# and VB compilers - `Microsoft.Net.Compilers`. To install it you can run the following command in the Package Manager Console:

    `nuget install Microsoft.Net.Compilers`

 - The Language APIs and Services - `Microsoft.CodeAnalysis`. To install it you can run the following command in the Package Manager Console:

    `nuget install Microsoft.CodeAnalysis`

Additionally it is a good to install the .NET Compiler Platform SDK Templates, that can be found [here][1]. This will get you:

 - Templates for both C# and Visual Basic that enable the creation of Analyzers, CodeFixes and stand-alone analysis tools.
 - The Syntax Visualizer tool for Visual Studio(`View -> Other Windows -> Syntax Visualizer`), which is extremely usefully for examining the syntax tree of existing code.

  [1]: https://go.microsoft.com/fwlink/?LinkID=526901

## Additional tools and resources
 - The Roslyn Quoter

A tool for converting an sample C# program to syntax tree API calls. The tool itself can be found [here][1].

- Enhanced source viewer 

An easy way to view the Roslyn source code can be found [here][2].


  [1]: http://roslynquoter.azurewebsites.net/
  [2]: http://source.roslyn.codeplex.com/

