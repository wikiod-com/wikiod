---
title: "Building libraries with .NET Core"
slug: "building-libraries-with-net-core"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Platform-specific dependencies
You can specify different dependencies for each platforms:

    "net45": {
        "frameworkAssemblies": {
            "System.Linq": "4.1.0"
        }
    },
    "netstandard1.3": {
        "dependencies": {
            "NETStandard.Library": "1.6.0",
            "System.Linq": "4.1.0-rc2"
        }
    },
    "netstandard1.4": {
        "dependencies": {
            "NETStandard.Library": "1.6.0",
            "System.Linq": "4.1.0"
        }
    }

When this project is compiled and packed, each framework target will use a different set of dependencies:

* `net45` (projects targeting .NET 4.5+) will use the `System.Linq` assembly from the GAC.
* `netstandard1.3` (.NET Core projects targeting .NET Standard 1.3) will use the `NETStandard.Library` version 1.6.0 NuGet package, and the `System.Linq` prerelease version 4.1.0-rc2 NuGet package.
* `netstandard1.4` will use the same version of `NETStandard.Library`, but the release 4.1.0 version of `System.Linq`.

## Targeting both .NET Standard and .NET Framework 4.x
    {
       "description": "My awesome library",
       "dependencies": { },
       "frameworks": {
          "net40": { },
          "netstandard1.3": {
             "dependencies": {
                "NETStandard.Library": "1.6.0"
             }
          }
       }
    }

By targeting both `net40` and `netstandard1.3`, the library will work in both .NET 4.0+ projects and .NET Standard projects. It's important to move the `NETStandard.Library` dependency into the `netstandard1.3` section so it's only referenced when building for that framework.

## Producing a NuGet package for a library
Any project that targets `netstandard1.X` can be packed into a NuGet package by running:

    dotnet pack

The resulting package can be uploaded to [NuGet][nuget], [MyGet][myget], or hosted in a [local package source][local].

[nuget]: https://www.nuget.org/
[myget]: https://www.myget.org/
[local]: https://docs.nuget.org/create/hosting-your-own-nuget-feeds

## Targeting .NET Standard
    {
       "description": "My awesome library",
       "dependencies": {
          "NETStandard.Library": "1.6.0"
       },
       "frameworks": {
          "netstandard1.3": { }
       }
    }

A library that targets `netstandard1.3` can be used on any framework that supports .NET Standard 1.3 **or later**. Choosing a lower .NET Standard version for a library means that more projects can use it, but less APIs are available.

## Adding a project reference to a library
If you have multiple libraries in the same solution, you can add local (project) references between them:

    {
       "dependencies": {
          "NETStandard.Library": "1.6.0",
          "MyOtherLibrary": {
            "target": "project"
          }
       },
       "frameworks": {
          "netstandard1.3": { }
       }
    }

The `target: project` property value tells NuGet to look in the current solution for `MyOtherLibrary`, instead of in your package sources.

