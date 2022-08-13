---
title: "Getting started with haxe"
slug: "getting-started-with-haxe"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
## Requirements

 1. A version of the Haxe toolkit must be installed
 2. Haxe must be present in your system path
 2. Command line must be accessible

## Code

Navigate to a desired project directory and create a `Test.hx` source file with the following content:

    class Test {
        static function main() {
            trace("Hello world");
        }
    }

Haxe source files are called **modules**. A module _should_ define a type (`abstract`, `class`, `enum`, `interface`, or `typedef`) with the same identifier as the module name - in this case the `Test` class. Once that requirement is met, a module can define an arbitrary number of different types.

Haxe programs require an **entry point**, as denoted by the static `main` function. The class implementing the entry point is the **startup class** or main class. Again, in this case the main class is the `Test` class.

The `trace()` function is a general purpose logging function exposed to the global namespace for the sake of convenience. It outputs to the target language's standard output handle (e.g. browser console for JavaScript, command line for C++). See the [API documentation][1] for more information.

## Execution

Navigate to the project folder from your command line. Test to see if Haxe is configured in your environment by calling:

`haxe --help`

The Haxe interpreter can be used to test code that does not rely on any specific target language API. Use the interpreter by calling:

`haxe -main Test --interp`

_Remember_, the `Test` module contains the `Test` startup class, which is why `-main Test` is passed to the compiler.

Haxe sources can compile (_transpile_) to sources / bytecodes of several different languages. The following table displays the target language, compiler flag, argument type, and compilation result. Use it by calling:

`haxe -main Test [flag] [argument]`.

| Language       | Flag    | Argument  | Result                           |
| -------------- | ------- | --------- | -------------------------------- |
| ActionScript 3 | -as3    | Directory | Source                           |
| C#             | -cs     | Directory | Source + optional bytecode (.exe)|
| C++            | -cpp    | Directory | Source + optional binary (native)|
| Flash          | -swf    | File      | Bytecode (.swf)                  |
| HL             | -hl     | File      | Source                           |
| Lua            | -lua    | File      | Source                           |
| Java           | -java   | Directory | Source + optional bytecode (.jar)|
| JavaScript     | -js     | File      | Source                           |
| Neko           | -neko   | File      | Bytecode (.n)                    |
| PHP            | -php    | Directory | Source                           |
| Python         | -python | File      | Source                           |
| HashLink       | -hl     | File      | Bytecode (.hl)                   |

Note that the path arguments here are relative to the path `haxe` was called from. The optional bytecode/binary outputs can be opt-outed by adding the `-D no-compilation` flags, in order to avoid an additional compilation step involving calling the target language's compiler.

## References

 - [API documentation for `haxe.Log`][1]
 - ["Hello world" entry in the Haxe Code Cookbook][2]


  [1]: http://api.haxe.org/haxe/Log.html
  [2]: http://code.haxe.org/category/beginner/hello-world.html

## Installation
Haxe is [available][1] on Windows, Linux, and OS X. It is distributed in two forms:

 - as an **installer**, providing an optional Neko VM dependency and configuring `haxe` and `haxelib` environment variables;
 - as **binaries**, providing only the Haxe compiler and package manager.

## Windows

Installer and binaries are available from the [Haxe website][1].

## Linux

Binaries (32-bit and 64-bit) are available from the [Haxe website][1].

The Haxe Foundation also officially participates in the maintenance of Haxe and Neko packages for [popular Linux distributions][2]. It is recommended to use those packages if available.

### Ubuntu

It is recommended to use the [Haxe PPA][3] which provides latest Haxe and Neko releases for all currently supported Ubuntu versions. The PPA can also be used for Ubuntu-based distributions.

    sudo add-apt-repository ppa:haxe/releases -y
    sudo apt-get update
    sudo apt-get install haxe -y
    mkdir ~/haxelib && haxelib setup ~/haxelib

Note that Neko is installed as a dependency of Haxe.

### Debian

To install the currently available stable versions, run the following commands:

    sudo apt-get install haxe -y
    mkdir ~/haxelib && haxelib setup ~/haxelib

Note that Neko will be installed as a dependency of Haxe.

To install newer releases from the unstable channel, do the following:

 1. In `/etc/apt/sources.list`, add

    `deb http://httpredir.debian.org/debian unstable main contrib non-free`

 2. In `/etc/apt/preferences.d/`, create a new file named `unstable` with the following content:

    ```
    Package: *
    Pin: release a=unstable
    Pin-Priority: 100
    
    Package: haxe neko libneko*
    Pin: release a=unstable
    Pin-Priority: 999
    ```
 3. Pull package index files from the newly added source:

    `sudo apt-get update`

 4. Install Haxe (and Neko):

    `sudo apt-get install haxe -y`

### Fedora

The Haxe Foundation maintains the Haxe and Neko RPM packages in the Fedora repository. The packages are up-to-date most of the time. However, when a new version of Haxe is released, it will take a few days, up to 2 weeks, to push an updated package to the stable releases of Fedora. The update activities can be tracked in the [Bodhi Fedora Update System][4].

To install the currently available versions of Haxe and Neko, run the following commands:

    sudo dnf install haxe -y
    mkdir ~/haxelib && haxelib setup ~/haxelib

Note that Neko is installed as a dependency of Haxe.
    
### openSuse

The Haxe Foundation maintains the Haxe and Neko RPM packages in the openSUSE:Factory repository. The packages are up-to-date most of the time. However, when a new version of Haxe is released, it will take a few days, up to 2 weeks, to be accepted by openSUSE:Factory.

To install currently available versions of Haxe and Neko, run the following commands:

    sudo zypper install haxe
    mkdir ~/haxelib && haxelib setup ~/haxelib

Note that Neko is installed as a dependency of Haxe.

To get the lastest Haxe version that may not available to openSUSE:Factory or an openSUSE release, use the [devel:languages:haxe][5] project in the openSUSE Build Service. Visit the [Haxe package page][6], click "Download package" at the top-right corner and follow the instructions. Again, Neko will also be installed as a dependency of Haxe.

### Arch Linux

There are Haxe and Neko packages in the Arch Linux community repository. The Haxe Foundation will continue to help keep the packages up-to-date. However, when a new version of Haxe is released, it will take time to update the package, depended on the availability of the package maintainer.

For currently available versions of Haxe and Neko, check the following pages:

 - [Haxe in Arch Linux][7]
 - [Neko in Arch Linux][8]

To install the currently available versions of Haxe and Neko, run the following commands:

    sudo pacman -S haxe
    mkdir ~/haxelib && haxelib setup ~/haxelib

Note that Neko is installed as a dependency of Haxe.

## OS X

Installer and binaries are available from the [Haxe website][1].  

It is also possible to install the current stable Haxe version through the [Brew][9] package manager.  

    brew install haxe

## References

 - ["Downloads", Haxe website][1]
 - ["Linux Software Packages", Haxe website][2]


  [1]: http://haxe.org/download/
  [2]: https://haxe.org/download/linux
  [3]: https://launchpad.net/~haxe/+archive/ubuntu/releases
  [4]: https://bodhi.fedoraproject.org/updates/?packages=haxe
  [5]: https://build.opensuse.org/project/show/devel:languages:haxe
  [6]: https://build.opensuse.org/package/show/devel:languages:haxe/haxe
  [7]: https://www.archlinux.org/packages/?q=haxe
  [8]: https://www.archlinux.org/packages/?q=neko
  [9]: http://brew.sh/

