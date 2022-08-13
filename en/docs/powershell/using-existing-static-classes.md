---
title: "Using existing static classes"
slug: "using-existing-static-classes"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

These classes are reference libraries of methods and properties that do not change state, in one word, immutable. You don't need to create them, you simply use them. Classes and methods such as these are called static classes because they are not created, destroyed, or changed.You can refer to a static class by surrounding the class name with square brackets.

## Using the .Net Math Class
You can use the .Net Math class to do calculations ([System.Math])

If you want to know which methods are available you can use:

    [System.Math] | Get-Member -Static -MemberType Methods

Here are some examples how to use the Math class:

    PS C:\> [System.Math]::Floor(9.42)
    9
    PS C:\> [System.Math]::Ceiling(9.42)
    10
    PS C:\> [System.Math]::Pow(4,3)
    64
    PS C:\> [System.Math]::Sqrt(49)
    7



## Adding types
By Assembly Name, add library

    Add-Type -AssemblyName "System.Math"

or by file path:

    Add-Type -Path "D:\Libs\CustomMath.dll"

To Use added type:

    [CustomMath.NameSpace]::Method(param1, $variableParam, [int]castMeAsIntParam)

## Creating new GUID instantly
Use existing .NET classes instantly with PowerShell by using [class]::Method(args):
 
    PS C:\> [guid]::NewGuid()

    Guid
    ----
    8874a185-64be-43ed-a64c-d2fe4b6e31bc

Similarly, in PowerShell 5+ you may use the `New-Guid` cmdlet:

    PS C:\> New-Guid

    Guid
    ----
    8874a185-64be-43ed-a64c-d2fe4b6e31bc


To get the GUID as a `[String]` only, referenced the `.Guid` property:

    [guid]::NewGuid().Guid


