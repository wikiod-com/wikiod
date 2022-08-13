---
title: "Understanding System.Runtime vs. mscorlib"
slug: "understanding-systemruntime-vs-mscorlib"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

Every .NET library and programming language utilize a set of elementary data types like `System.Int32`, `System.Object`, `System.Type` or `System.Uri`. These data types form the base of all other structures including all custom written .NET libraries. All these types are hosted in a base library, which is either `mscorlib` or `System.Runtime`.

The libraries which can be used with .NET Core are based on `System.Runtime` core library while for the .NET Framework (the Windows component) they are based on `mscorlib`. This essential difference lead to...

 - the incompatibility of older libraries since they expect a type `System.Object, mscorlib` while a .NET Core library would expect `System.Object, System.Runtime`.
 - a type forwarding facade library called `System.Runtime` to the `mscorlib` in the **.NET Framework**. This library is otherwise (nearly) empty but enables the usage of `System.Runtime` based PCL libraries on the .NET Framework.
 - a type forwarding `mscorlib` to the `System.Runtime` in a **future** version of **.NET Core**.
 - the introduction of the portable class library concept (PCL) and as a second generation the `netstandard` as a method of unification between the two core libraries.

**AND** out of that, countless questions on Stack Overflow.

## Popular Error: Misleading NuGet the wrong way
.NET Core `project.json` supports NuGet importing (a.k.a. lying according to [this SO answer](http://stackoverflow.com/a/38688430/2410379)). It is impossible to include a `mscorlib` based library due to an `import` statement.

    {
        "version": "1.0.0-*",

        "dependencies": {
            "Microsoft.AspNet.Identity.EntityFramework": "2.2.1",
            "NETStandard.Library": "1.6.0"
        },

        "frameworks": {
            "netstandard1.6": {
                "imports": [
                    "net461"
                ]
            }
        }
    }

Imports only work with portable class libraries (which are `System.Runtime` based) or deprecated target framework monikers which are also `System.Runtime` based (e.g. `dotnet` or `dnxcore`)


## Popular Error: Add a NuGet package which was not made for netstandard / netcoreapp (System.Runtime)
In the example `project.json` below, an assembly `Microsoft.AspNet.Identity.EntityFramework` was added which is `mscorlib` based.

    {
        "version": "1.0.0-*",

        "dependencies": {
            "Microsoft.AspNet.Identity.EntityFramework": "2.2.1",
            "NETStandard.Library": "1.6.0"
        },

        "frameworks": {
            "netstandard1.6": { }
        }
    }

The author of the assembly `Microsoft.AspNet.Identity.EntityFramework` has not ported the NuGet package yet to `netstandard` (they actually did it, they just renamed the package as well `Microsoft.AspNetCore.Identity.EntityFrameworkCore` ;))

If you encounter a package you need and is not yet on netstandard, please contact the author and - if possible - help porting it.

## Popular Error: Misunderstanding the outcome
Targeting multiple frameworks with `project.json` is simple. However the result are two different
compilations. Take the following example:

    {
        "version": "1.0.0-*",

        "dependencies": {
            "NETStandard.Library": "1.6.0",
            "System.Collections.Immutable": "1.2.0"
        },

        "frameworks": {
            "netstandard1.3": { },
            "net451": { }
        }
    }

The compilation process for the `project.json` file will lead to two resulting artifacts:

 - One compiled dll for the `System.Runtime` based `netstandard` world which can be used on .NET Core, .NET Framework (via type forwarders) and Xamarin products (via type forwarders). This dll has references to `System.Runtime` and `System.Collections.Immutable`.
 - Another compiled dll directly for the `mscorlib` based .NET Framework. This dll  will have references to `mscorlib` and `System.Collection.Immutable`.

However, it is important to understand that the `netstandard1.0` based `System.Collections.Immutable` will utilize different `System.Runtime` implementations for each build dll at runtime. The `System.Runtime` which comes with .NET Core does not have any assembly dependencies on its own (since it implements the core library). The `System.Runtime` used for with the .NET Framework has references (for the type forwarders) to the .NET Framework assemblies `mscorlib`, `System.Core`, `System` and `System.ComponentModel.Composition`.

## Popular Error: Accidently adding a mscorlib library as a dependencies to a netstandard/netcoreapp
Another popular error is the referring of packages which does not satisfy all framework on the 
global scope when multiple frameworks are targeted.


    {
        "version": "1.0.0-*",

        "dependencies": {
            "NETStandard.Library": "1.6.0",
            "Microsoft.AspNet.Identity.EntityFramework": "2.2.1"
        },

        "frameworks": {
            "netstandard1.3": { },
            "net451": { }
        }
    }

The (meta) library `NETStandard.Library` works fine in this example, since it targets 
both `netstandard1.3` and `net451`. However the library 
`Microsoft.AspNet.Identity.EntityFramework` does only target the .NET Framework `net` and `mscorlib`
and therefore cannot be used for a `netstandard` output.

Either search for a library (version) which cover both frameworks or add the library
in the conditional dependencies below the framework.

    {
        "version": "1.0.0-*",

        "dependencies": {
            "NETStandard.Library": "1.6.0"
        },

        "frameworks": {
            "netstandard1.3": { },
            "net451": {
                "dependencies": {
                    "Microsoft.AspNet.Identity.EntityFramework": "2.2.1",
                }
            }
        }
    }

In this case, the library can only be used in conditional #ifdef blocks for the `net451` build.

