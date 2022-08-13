---
title: "F# on .NET Core"
slug: "f-on-net-core"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Creating a new project via dotnet CLI
Once you've installed the .NET CLI tools, you can create a new project with the following command:

    dotnet new --lang f#

This creates a command line program.

## Initial project workflow
Create a new project

    dotnet new -l f#

Restore any packages listed in project.json

    dotnet restore

A project.lock.json file should be written out.

Execute the program

    dotnet run

The above will compile the code if required.

The output of the default project created by `dotnet new -l f#` contains the following:

    Hello World!
    [||]





