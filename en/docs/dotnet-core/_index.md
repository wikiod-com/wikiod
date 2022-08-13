---
title : .net-core Tutorial
slug : net-core-tutorial
weight : 9902
draft : false
images : []
type : docs
---

**.NET Core** is a general purpose development platform maintained by Microsoft and the .NET community on GitHub.

The following characteristics best define .NET Core:

- Flexible deployment: Can be included in your app or installed side-by-side user- or machine-wide.
- Cross-platform: Runs on Windows, macOS and Linux; can be ported to other OSes. The supported Operating Systems (OS), CPUs and application scenarios will grow over time, provided by Microsoft, other companies, and individuals. .NET can also be used in device, cloud, and embedded/IoT scenarios.
- Command-line tools: All product scenarios can be exercised at the command-line.
- Compatible: .NET Core is compatible with .NET Framework, Xamarin and Mono, via the .NET Standard Library.
- Open source: The .NET Core platform is open source, using MIT and Apache 2 licenses. Documentation is licensed under CC-BY. .NET Core is a .NET Foundation project.
- Supported by Microsoft: .NET Core is supported by Microsoft, per .NET Core Support

Composition
-----------

**.NET Core** is composed of the following parts:

- **A .NET runtime** which provides a type system, assembly loading, a garbage collector, native interop, and other basic services.
- **A set of framework libraries** which provide primitive data types, app composition types, and fundamental utilities.
- **A set of SDK tools and language compilers** that enable the base developer experience, available in the .NET Core SDK.
- **The 'dotnet' app host** which launches .NET Core apps. The app host selects and hosts the runtime, provides an assembly loading policy, and launches the app. The same host is also used to launch SDK tools in a similar fashion.

(Source: [official documentation][1].)


  [1]: https://docs.microsoft.com/en-us/dotnet/articles/core/index

