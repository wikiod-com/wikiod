---
title: ".NET Core command line interface"
slug: "net-core-command-line-interface"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Creating a NuGet package
To create a NuGet package from a project, run this command from a directory that contains **project.json**:

    dotnet pack

The resulting `.nupkg` file will be named and versioned according to the properties in **project.json**. If there are multiple frameworks targeted in the project file, the package will support all of them.

## Publish and run a .NET Core project
Go to the **project.json** directory and publish:

    dotnet publish

It will print the output directory of the operation, enter the directory and run the published project:

    dotnet <project output>.dll

The default folder will be: `<project root>/bin/<configuration>/<target framework>/publish`

For example: `example/bin/Debug/netcoreapp1.0/publish`

If you have built the project previously, you can publish using:

    dotnet --no-build publish

**Important:** Make sure you publish the project from the same user who restored the packages or you might publish it without the required libraries.

You can specify the configuration with the `-c <Configuration>` option. To publish in Release mode, use `dotnet publish -c Release`.

## Create .NET Core "Hello World" console project
Create a new **project.json** and example **Program.cs**:

    dotnet new

Restore needed packages:

    dotnet restore

Compile and run the example:

    dotnet run



## Scaffolding other project types
Using `dotnet new` will scaffold a new console application. To scaffold other types of projects, use the `-t` or `--type` flag:

    dotnet new -t web
    dotnet restore
    dotnet run

The available templates vary by language.

## C# Templates

* `console` (default) - A console application.
* `web` - An ASP.NET Core application.
* `lib` - A class library.
* `xunittest` - An xUnit test project.

## F# Templates
* `console` (default) - A console application.
* `lib` - A class library.

## Scaffolding projects in other languages
By default, `dotnet new` creates C# projects. You can use the `-l` or `--lang` flag to scaffold projects in other languages:

    dotnet new -l f#
    dotnet restore
    dotnet run

Currently, `dotnet new` supports C# and F#.

## Running automated tests
Running `dotnet test` from inside a folder that contains a test project will launch the test runner. The test runner will discover and run the tests in the project.

To be compatible with `dotnet test`, the **project.json** file must contain a `testRunner` property and a dependency on a compatible test runner package:


    {
      "dependencies": {
        "dotnet-test-xunit": "2.2.0-preview2-build1029",
        "Microsoft.NETCore.App": {
          "type": "platform",
          "version": "1.0.0"
        },
        "xunit": "2.1.0"
      },
      "frameworks": {
        "netcoreapp1.0": {
          "imports": [ "dotnet", "portable-net45+win8" ]
        }
      },
      "testRunner": "xunit"
    }





