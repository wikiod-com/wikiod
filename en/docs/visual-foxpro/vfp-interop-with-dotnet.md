---
title: "VFP Interop with .NET"
slug: "vfp-interop-with-net"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This topic will cover interop between VFP and .NET.

## Using wwDotNetBridge to Run .NET Code
With the help of [West Wind's wwDotNetBridge][1], you can easily have access .NET code within a VFP program.

The [white paper][2] has all the details, but this concise example will help illustrate the basic steps to running a method in a .NET assembly.

Note that wwDotNetBridge can directly access simple properties like strings, ints, etc. In order to access more complicated structures like lists, you first need to use the wwDotNetBridge function CreateArray to convert the .NET structure to a VFP COM array (as shown at the bottom of this example).

    *!* Load WestWind .NET wrapper library (wwdotnetbridge.prg assumed to be in the search path)
    IF (!wwDotNetBridge())
        RETURN .F.
    ENDIF

    lowwDotNetBridge = CREATEOBJECT("wwDotNetBridge","V4")

    *!* Load .NET Assembly (include full or relative path if necessary)
    IF !lowwDotNetBridge.LoadAssembly("SomeDotNetAssembly.dll")
        lcAssemblyLoadError = "LoadAssembly error: " + lowwDotNetBridge.cErrorMsg
        =MESSAGEBOX(lcAssemblyLoadError, MB_ICONSTOP, "Error")
        RETURN .F.
    ENDIF

    *!* Parameters to pass to class constructor
    *!* You can pass up to 5 paramenters to the constructor
    lcParameter1 = "StringParameter1"
    lcParameter2 = "StringParameter2"
    lnParameter3 = 3
    lcParameter4 = .NULL.

    *!* Get an instance of the assembly class
    loAssemblyReference = lowwDotNetBridge.CreateInstance("MyDotNetProject.MyDotNetClass", ;
        lcParameter1, lcParameter2, lnParameter3, lcParameter4)
    IF lowwDotNetBridge.lError
        lcAssemblyLoadError = "An error occurred loading the class: " + lowwDotNetBridge.cErrorMsg
        RETURN .F.
    ENDIF

    *!* Usage Example

    *!* This example runs a method that return a boolean 
    *!* and populates a List<string> (SomeStringList).
    *!*
    *!* The assembly has a public property named "LastErrorMessage" 
    *!* with details about any handled exceptions/problems.

    IF (!loAssemblyReference.SomePublicMethod())
        msg = "There was a problem executing the method:" + CRLF + ;
            loAssemblyReference.LastErrorMessage
        =MESSAGEBOX(msg, MB_ICONSTOP, "Error")
        RETURN .F.
    ENDIF

    *!* At this point the string list (SomeStringList) should be populated
    *!* wwDotNetBridge can convert that list to a VFP COM array (0-based)

    laVFPArrayOfStrings = lowwDotNetBridge.CreateArray()
    laVFPArrayOfStrings.FromEnumerable(loAssemblyReference.SomeStringList)

    FOR x = 0 TO laVFPArrayOfStrings.Count-1
        ? laVFPArrayOfStrings.Item(x)
    ENDFOR


  [1]: http://west-wind.com/wwdotnetbridge.aspx
  [2]: http://west-wind.com/presentations/wwdotnetbridge/wwdotnetbridge.pdf

