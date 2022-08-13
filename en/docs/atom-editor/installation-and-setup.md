---
title: "Installation and Setup"
slug: "installation-and-setup"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

To troubleshoot errors that occur with building from source, please view the [build documents](https://github.com/atom/atom/tree/master/docs/build-instructions).

## Installing Atom on Windows
# Using the official installer
Download the installer from [the official website](https://atom.io/). It will automatically add `atom` and `apm` (Atom Package Manager) to your `%PATH%` variable.

# Building from source

**Requirements**:
 - Node.js 4.4.x or later
 - Python 2.7.x
 - 7zip
 - Visual Studio (One of the versions below)
     - Visual C++ Build Tools 2015
     - Visual Studio 2013 Update 5 (Express Edition or better)
     - Visual Studio 2015 (Community Edition or better)
 - Git

Run the following commmands into Command Prompt:

    cd C:/
    git clone https://github.com/atom/atom.git
    cd atom
    script/build

## Installing Atom on Mac
# Installing from a zip

 1. Download the `atom-mac.zip` zip file from the Atom GitHub repository [here](https://github.com/atom/atom/releases)
 2. Unzip the file by double clicking on it in Finder
 3. Drag the `Atom` application into your "Applications" folder
 4. Run the Atom application.

# Building from Source

**Requirements:**
 - macOS 10.8 or higher
 - Node.js 4.4x or later
 - npm 3.10.x or later
 - Xcode

Installation:

    git clone https://github.com/atom/atom.git
    cd atom
    script/build

After building, install with `script/build --install`

## Installing Atom on Linux
# Installing from a package

## Debian, Ubuntu, etc.

    $ sudo dpkg -i atom-amd64.deb
    $ sudo apt-get -f install

## RedHat Enterprise, CentOS, Oracle Linux, Scientific Linux, etc.

    $ sudo yum install -y atom.x86_64.rpm

## Fedora (DNF package manager)

    $ sudo dnf install -y atom.x86_64.rpm

## SUSE (Zypp package manager)

    $ sudo zypper in -y atom.x86_64.rpm

# Building from Source

**Requirements**:
 - OS with 64 or 32 bit architecture
 - C++ 11 toolchain
 - Git
 - Node.js 4.4x or later
 - npm 3.10.x or later
 - GNOME Keyring Development headers

Run the following commands:

    git clone https://github.com/atom/atom.git
    cd atom
    script/build

For specific instructions related to a single Linux distro, read [these](https://github.com/atom/atom/blob/master/docs/build-instructions/linux.md) instructions.

