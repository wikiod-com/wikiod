---
title: "Components and Versioning in .NET Core"
slug: "components-and-versioning-in-net-core"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

This document covers the different components that make up a .NET Core distribution and how they are versioned. This document currently covers the 1.x releases.



How components in .NET Core are versioned.

# Components

.NET Core consists of multiple components that are each versioned
independently and can often be mixed and matched.

- **Shared Framework**. This contains the APIs and the Virtual Machine
  and other runtime services needed
  for running .NET Core applications.

  - The current .NET Core Virtual Machine is called **CoreCLR**. This
    executes the .NET bytecode by compiling it JIT and provides
    various runtime services including a garbage collector. The
    complete source code for CoreCLR is available at
    https://github.com/dotnet/coreclr.

  - The .NET Core standard APIs are implemented in **CoreFX**. This
    provides implementations of all your favourite APIs such as
    `System.Runtime`, `System.Theading` and so on. The source code for
    CoreFX is available at https://github.com/dotnet/corefx.

- **Host** is also called the *muxer* or *driver*. This components
  represents the `dotnet` command and is responsible for deciding what
  happens next. The source for this is available at
  https://github.com/dotnet/core-setup.
  
- **SDK** is also sometimes called the *CLI*. It consists of the
  various tools (`dotnet` subcommands) and their implementations that
  deal with building code. This includes handling the restoring of
  dependencies, compiling code, building binaries, producing packages
  and publishing standalone or framework dependent packages. The SDK
  itself consists of the *CLI*, which handles command line operations
  (at https://github.com/dotnet/cli) and various subprojects that
  implement the various operations the CLI needs to do.


# Components in .NET Core installations

Various official and unoffical packages, tarballs, zips and installers
for .NET Core (including those available on https://dot.net/core)
provide .NET Core in many variants. Two common ones are SDKs and
Runtimes.

Each *SDK install* or *Runtime install* contains a number (possibly 0)
of hosts, sdk and shared framework components described above.

- .NET Core **Runtime** contains
  - 1 version of *Shared Framework*
  - 1 version of the *Host*

- .NET Core **SDK** contains
  - 1 or more versions of the *Shared Framework* (varies depending on
    the version of the *SDK*)
  - 1 version of the *Host*
  - 1 version of the *SDK*

## Identifying Versions

Each .NET Core component (SDK, Host and Shared Framework) is versioned independently.

You can find the version for each of them separately.

- **SDK**

  You can use the `--version` option to `dotnet` to see the SDK
  version. For example:

    ```
    $ ~/dotnet-1.1.1/dotnet --version
    1.0.0-preview2-1-003176
    ```

  `dotnet --info` also shows the SDK version.

- **Host**

  You can run `dotnet` by itself without any arguments or options to
  see the version of the host.

    ```
    $ ~/dotnet-1.1.1/dotnet

    Microsoft .NET Core Shared Framework Host
    
      Version  : 1.1.0
      Build    : 362e48a95c86b40cd1f2ef3d08741f7fed897956
    
    Usage: dotnet [common-options] [[options] path-to-application]
    ...
    ```
  
- **Shared Framework**

  There no command currently to display the avaialble shared
  frameworks. I use `ls
  /path/to/where/you/installed/dotnet/shared/Microsoft.NETCore.App`
  which relies on internal implementation details. For example:

    ```
    $ ls ~/dotnet-1.1.1/shared/Microsoft.NETCore.App/
    1.1.1
    ```

## Selecting Versions
It's possible to have multiple .NET Core *SDK*s and *Runtimes*
available on disk. You can select the versions for each separately.

To select the version of the SDK to use, use [`global.json`](https://docs.microsoft.com/en-us/dotnet/articles/core/tools/global-json). 

To select the version of the shared framework to use, target the specified framwork in the [`.csproj` file (or `project.json` if you are still using that)](https://docs.microsoft.com/en-us/dotnet/articles/core/tools/project-json-to-csproj#frameworks).

