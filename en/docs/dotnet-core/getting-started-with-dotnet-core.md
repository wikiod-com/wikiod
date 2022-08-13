---
title: "Getting started with .net-core"
slug: "getting-started-with-net-core"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Building a Hello World Sample Application
Create an empty directory somewhere ...

    mkdir HelloWorld
    cd HelloWorld

Then use the built in scaffolding technology to create a Hello World sample

    dotnet new console -o 

This command creates two files:

 - `HelloWorld.csproj` describes the project dependencies, settings, and Target Framework
 - `Program.cs` which defines the source code for the main entry point and the console emitting of "Hello World".

If the `dotnet new` command fails, make sure you have installed .NET Core properly. Open the `Program.cs` file in your favorite editor to inspect it:

    namespace ConsoleApplication
    {
        public class Program
        {
            public static void Main(string[] args)
            {
                Console.WriteLine("Hello World!");
            }
        }
    }

To restore the project dependencies and the .NET runtime, execute

    dotnet restore

To compile the application and execute it, enter

    dotnet run

This last command prints "Hello World" to the console.

## Installation from a binary archive
*Note: These instructions are targeted at .NET Core 1.0.4 & 1.1.1 SDK 1.0.1 and higher.*

When using binary archives to install, we recommend the contents be extracted to /opt/dotnet and a symbolic link created for dotnet. If an earlier release of .NET Core is already installed, the directory and symbolic link may already

```
sudo mkdir -p /opt/dotnet
sudo tar zxf [tar.gz filename] -C /opt/dotnet
sudo ln -s /opt/dotnet/dotnet /usr/local/bin
```
**Ubuntu installation**
```
dotnet-host-ubuntu-x64.deb
dotnet-hostfxr-ubuntu-x64.deb
dotnet-sharedframework-ubuntu-x64.deb
dotnet-sdk-ubuntu-x64.1.0.1.deb
```
**Set up package source**

The first step is to establish the source feed for the package manager. This is only needed if you have not previously set up the source or if you are installing on Ubuntu 16.10 for the first time.

**Ubuntu 14.04 and Linux Mint 17**

*Commands*
```
sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 417A0893
sudo apt-get update
sudo apt-get install dotnet-dev-1.0.1
```

*Installed packages*

```
dotnet-host-ubuntu-x64.1.0.1.deb
dotnet-hostfxr-ubuntu-x64.1.0.1.deb
dotnet-sharedframework-ubuntu-x64.1.1.1.deb
dotnet-sdk-ubuntu-x64.1.0.1.deb
```

**Ubuntu 16.04 and Linux Mint 18**

*Commands*
```
sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ xenial main" > /etc/apt/sources.list.d/dotnetdev.list'
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 417A0893
sudo apt-get update
sudo apt-get install dotnet-dev-1.0.1
```

*Installed packages*

```
dotnet-host-ubuntu.16.04-x64.1.0.1.deb
dotnet-hostfxr-ubuntu.16.04-x64.1.0.1.deb
dotnet-sharedframework-ubuntu.16.04-x64.1.1.1.deb
dotnet-sdk-ubuntu.16.04-x64.1.0.1.deb
```

**Ubuntu 16.10**

*Commands*
```
sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ yakkety main" > /etc/apt/sources.list.d/dotnetdev.list'
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 417A0893
sudo apt-get update
sudo apt-get install dotnet-dev-1.0.1
```

*Installed packages*

```
dotnet-hostfxr-ubuntu.16.10-x64.1.0.1.deb
dotnet-host-ubuntu.16.10-x64.1.0.1.deb
dotnet-sharedframework-ubuntu.16.10-x64.1.1.1.deb
dotnet-sdk-ubuntu.16.10-x64.1.0.1.deb
```

*source*
[Official Documentation][1] 


  [1]: https://github.com/dotnet/core/blob/master/release-notes/download-archives/1.1.1-download.md

## Installation or Setup
Install .NET Core on macOS 10.11+, after install homebrew:

    brew update
    brew install openssl
    mkdir -p /usr/local/lib
    ln -s /usr/local/opt/openssl/lib/libcrypto.1.0.0.dylib /usr/local/lib/
    ln -s /usr/local/opt/openssl/lib/libssl.1.0.0.dylib /usr/local/lib/

Install .NET Core SDK from https://go.microsoft.com/fwlink/?LinkID=835011

[Official Microsoft .NET Core page][1] with installation guides for Windows, Linux, Mac and Docker

Detailed instructions on getting .net-core set up or installed.


  [1]: https://www.microsoft.com/net/core

