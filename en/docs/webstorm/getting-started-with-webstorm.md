---
title: "Getting started with webstorm"
slug: "getting-started-with-webstorm"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
# Background
============

[**WebStorm**][1] is lightweight yet powerful Integrated Development Environment (**IDE**) perfectly equipped for complex client-side development and server-side development, it is cross-platform and works on Windows, Mac OS X, and Linux.

WebStorm features advanced support for **JavaScript**, **HTML**, **CSS**, and their modern successors, as well as for frameworks such as [AngularJS][2] or [React][3], **debugging**, and **integration with the VCS** and various web development tools. also, it provides configured and ready-to-use local development environment, including support for [Node.js][4], [Meteor][5], [CoffeeScript][6], [TypeScript][7], [Dart][8], [Sass][9], and more.

<br>

# Hardware Requirements
=======================

In order to run WebStorm smoothly, the following hardware requirements are required:

 - 1 GB RAM minimum, 2 GB RAM recommended
 - 1024x768 minimum screen resolution

<br>

# System Requirements
=====================

For previous versions of **WebStorm before 2016** it's required to have Java installed on the machine in order to run WebStorm, starting from **WebStorm 2016 and above**, JRE 1.8 is bundled with distributions for all platforms. You don't need any Java to be installed on your machine in order to run WebStorm.

For the Operating System (**OS**) the following is required:

 - Microsoft Windows 10/8/7/Vista/2003/XP (incl.64-bit)
 - OS X 10.5 or higher, including 10.10 (Only 64-bit OS X is supported)
 - OS Linux 64 bit (KDE, GNOME or Unity DE desktop)

<br>

# Windows installation:

 1. Download WebStorm from the [Download][10] page.
 2. Run the `WebStorm-*.exe` file that starts the Installation Wizard. ( *the * means the current version downloaded* )
 3. Follow all steps suggested by the wizard. Pay special attention to the corresponding installation options.

<br>

# OS X installation:

 1. Download the `WebStorm-*.dmg` OS X Disk Image file from the [Download][11] page. ( *the * means the current version downloaded* )
 2. Double-click the downloaded `WebStorm-*.dmg` OS X Disk Image file to mount it.
 3. Copy WebStorm to your Applications folder.


<br>

# Linux installation:

 1. Download the `WebStorm-*.tar.gz` file from the [Download][12] page. ( the * means the current version downloaded )
 2. Unpack the WebStorm-*.tar.gz file to an empty directory using the following command:

    `tar xfz WebStorm-*.tar.gz`

 3. Because running WebStorm from wherever you downloaded the file to may be inconvenient, it is recommended that you move the extracted or unpacked archive folder to the desired location using the `mv` command in one of the following formats:

       `mv <path to extracted archive folder> <new archive folder>`

    

    Example
    -------

    - mv /downloads/WebStorm-* my/desired/location
    - mv <path to WebStorm-*.tar.gz> <new archive folder>

 1. Switch to the **bin** directory in the new location:

    `cd <new archive folder>/WebStorm-*/bin`


    Example
    -------

    - cd <new archive folder>/WebStorm-*/bin

 2. Run `webstorm.sh` from the **bin** subdirectory.

  [1]: https://www.jetbrains.com/webstorm/
  [2]: https://angularjs.org/
  [3]: https://facebook.github.io/react/
  [4]: https://nodejs.org/en/#download
  [5]: https://www.meteor.com/
  [6]: http://coffeescript.org/
  [7]: http://www.typescriptlang.org/
  [8]: https://www.dartlang.org/
  [9]: http://sass-lang.com/
  [10]: https://www.jetbrains.com/webstorm/download/#section=windows-version
  [11]: https://www.jetbrains.com/webstorm/download/#
  [12]: https://www.jetbrains.com/webstorm/download/#section=linux-version

## Create Command-line Launcher
A command-line launcher is a handle tool which allows one to open WebStorm using the command-line. It can easily be created by using the menus

    Tools > Create Command-line Launcher...

[![Screenshot of the "Create Command-line Launcher..." option in the menu][1]][1]

After selecting the option, you will be presented with the "Create Launcher Script" prompt for a location and name of the command-line launcher. This location should be within `$PATH` so that it can be invoked in the command prompt / terminal.

The default value is `/usr/local/bin/webstorm` where `/usr/local/bin` is the path the launcher will be added and `webstorm` is the name of the file. The name of the file will also be used as the command to invoke and launch WebStorm, it can change it, but for this example we will assume it is the default value of "webstorm"

[![Screenshot of "Create Launcher Script" prompt][2]][2]

After this is done, you can open a terminal or command prompt and user the "webstorm" command to launch WebStorm with a specific directory. In the example below, WebStorm will be opened with the directory `~/Workspace/my-project` at the root of the Project.

    > webstorm ~/Workspace/my-project

*Tip: Use `webstorm .` to open the current directory*.


  [1]: https://i.stack.imgur.com/gz2qZ.png
  [2]: https://i.stack.imgur.com/UblFq.png

