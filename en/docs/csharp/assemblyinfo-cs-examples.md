---
title: "AssemblyInfo.cs Examples"
slug: "assemblyinfocs-examples"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

The filename `AssemblyInfo.cs` is used by convention as the source file where developers place metadata attributes that describe the entire assembly they are building.

## Global and local AssemblyInfo


## [AssemblyVersion]
This attribute applies a version to the assembly.

    [assembly: AssemblyVersion("1.0.*")]

The `*` character is used to auto-increment a portion of the version automatically every time you compile (often used for the "build" number)

## [AssemblyTitle]
This attribute is used to give a name to this particular assembly.

    [assembly: AssemblyTitle("MyProduct")]



## [AssemblyProduct]
This attribute is used to describe the product that this particular assembly is for. Multiple assemblies can be components of the same product, in which case they can all share the same value for this attribute.

    [assembly: AssemblyProduct("MyProduct")]


## Automated versioning


## Common fields


## [InternalsVisibleTo]
If you want to make `internal` classes or functions of an assembly accessable from another assembly you declare this by `InternalsVisibleTo` and the assembly name that is allowed to access.


In this example code in the assembly `MyAssembly.UnitTests` is allowed to call `internal` elements from `MyAssembly`.

    [assembly: InternalsVisibleTo("MyAssembly.UnitTests")]

This is especially useful for unit testing to prevent unnecessary `public` declarations.

## Reading Assembly Attributes
Using .NET's rich reflection APIs, you can gain access to an assembly's metadata. For example, you can get `this` assembly's title attribute with the following code

    using System.Linq;
    using System.Reflection;
    
    ...
    
    Assembly assembly = typeof(this).Assembly;
    var titleAttribute = assembly.GetCustomAttributes<AssemblyTitleAttribute>().FirstOrDefault();
    
    Console.WriteLine($"This assembly title is {titleAttribute?.Title}");


## [AssemblyConfiguration]
AssemblyConfiguration: The AssemblyConfiguration attribute must have the configuration that was used to build the assembly.
Use conditional compilation to properly include different assembly configurations.
Use the block similar to the example below. Add as many different configurations as you commonly use.


    #if (DEBUG)
    
    [assembly: AssemblyConfiguration("Debug")]

    #else

    [assembly: AssemblyConfiguration("Release")]
    
    #endif


## [AssemblyKeyFile]
Whenever we want our assembly to install in GAC then it is must to have a strong name. For strong naming assembly we have to create a public key. 
To generate the `.snk` file.

To create a strong name key file

>  1. Developers command prompt for VS2015 (with administrator Access)
>  2. At the command prompt, type cd C:\Directory_Name and press ENTER.
>  3. At the command prompt, type sn -k KeyFileName.snk, and then press ENTER.

once the keyFileName.snk is created at specified directory then give refernce in your project . give `AssemblyKeyFileAttribute` attribute the path to `snk` file to generate the key when we build our class library.
    
> Properties  -> AssemblyInfo.cs
    
    [assembly: AssemblyKeyFile(@"c:\Directory_Name\KeyFileName.snk")]

Thi will create a strong name assembly after build. After creating your strong name assembly you can then install it in GAC

Happy Coding :) 

