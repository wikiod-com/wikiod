---
title : wix Tutorial
slug : wix-tutorial
weight : 9977
draft : false
images : []
type : docs
---

# What is WiX?

The [WiX toolset][1] lets developers create installers for Windows Installer, the Windows installation engine. It is [open source][2] and part of the [.NET Foundation][3].

The core of WiX is a set of build tools that build Windows Installer packages using the same build concepts as the rest of your product: source code is compiled and then linked to create executables; in this case .exe setup bundles, .msi installation packages, .msm merge modules, and .msp patches. The WiX command-line build tools work with any automated build system. Also, MSBuild is supported from the command line, Visual Studio, and Team Build.

WiX includes several extensions that offer functionality beyond that of Windows Installer. For example, WiX can install IIS web sites, create SQL Server databases, and register exceptions in the Windows Firewall, among others.

With Burn, the WiX bootstrapper, you can create setup bundles that install prerequisites like the .NET Framework and other runtimes along with your own product. Burn lets you download packages or combine them into a single downloadable .exe.

The WiX SDK includes managed and native libraries that make it easier to write code that works with Windows Installer, including custom actions in both C# and C++.

# How does WiX work?

The WiX source code is written in XML format with a .wxs file extension. The WiX tools follow the traditional compile and link model used to create executables from source code.

At build time, the WiX source files are validated against the core WiX schema, then processed by a preprocessor, compiler, and linker to create the final result. There are a set of WiX tools that can be used to produce different output types.

# WiX system requirements

WiX supports both .NET 3.5 and 4.0 and later. WiX's MSBuild supports requires .NET 3.5, which is not installed by default on Windows 8 and Windows Server 2012 and later.

In the next version of WiX (v3.11), .NET 4.0 will be required; building using .NET 3.5 will no longer be supported.

  [1]: http://wixtoolset.org/
  [2]: https://github.com/wixtoolset
  [3]: http://dotnetfoundation.org/

