---
title: "Getting started with intellij-idea"
slug: "getting-started-with-intellij-idea"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World!
This will teach you how to make your first project using IDEA.

Launch IDEA, and click `Create New Project` from the startup screen:
[![Startup screen][1]][1]
Click `Next` on the next screen. We're creating a simple Java project, so we don't need any addons or extras to this project
[![No Action Needed! Just click Next!][2]][2]
Use the next screen to create the `Java Hello World` template project:
[![Create from template][3]][3]
Finally, name your project and select a location on disk, and click `Finish`:
[![Name your project][4]][4]
You should end up with a window that looks something like this:
[![Main window][5]][5]
At this point, the project is all ready to go, simply click the `Run` button or go to `Run -> Run 'Main'`
[![Run your project][6]][6]
And you're done! The console will automatically pop up, giving it's salutations to the globe!
[![Hello World!][7]][7]


  [1]: http://i.stack.imgur.com/XhBw2.png
  [2]: http://i.stack.imgur.com/cFNeK.png
  [3]: http://i.stack.imgur.com/Xu4wA.png
  [4]: http://i.stack.imgur.com/phSA1.png
  [5]: http://i.stack.imgur.com/JSctK.png
  [6]: http://i.stack.imgur.com/JZQew.png
  [7]: http://i.stack.imgur.com/VFSHY.png

## Installation or Setup
There are two main versions of IntelliJ IDEA: the Community edition and the Ultimate edition. The Community edition is free and is not lacking for features in terms of Java SE development.

Windows & Linux
===============

Download IntelliJ IDEA from [the JetBrains website][1], and follow installation procedures. If the Java Development Kit (JDK) is not installed, [download and install the JDK][2]. Note that you need the JDK, only having the Java Runtime Enviroment (JRE) is not enough.

Once IntelliJ IDEA has been downloaded:

1. Run the installer
2. Press next
3. Choose a folder to install IntelliJ IDEA to (In most cases, leave this as the default)
4. Choose a start menu folder to crete IntelliJ IDEA shortcuts (In most cases, leave this as the default)
5. Choose whether to create a desktop shortcut, and choose whether to associate various Java files with IntelliJ IDEA
6. Press next, and wait for it to install

OS X / macOS
============

Download IntelliJ IDEA from [the JetBrains website][1], open the disk image (*.dmg) file downloaded, and drag and drop the application to the alias to your `/Applications` folder.

![Mac Installer][3]

Arch Linux
==========

IntelliJ IDEA can be installed on Arch Linux using its package manager, `pacman`. Open a terminal and enter the following command.

    sudo pacman -S intellij-idea-community-edition

Using `sudo` is not required if you're running as the root user.

[![Arch Linux Installation][4]][4]

Ubuntu
======

(1) Install ubuntu-make package.

For ubuntu 16.04 and later,

    sudo apt install ubuntu-make

For previous versions of ubuntu,
    
    sudo add-apt-repository ppa:ubuntu-desktop/ubuntu-make  
    sudo apt-get update
    sudo apt-get install ubuntu-make

(2) After installing Ubuntu Make, do a

    umake ide idea

Default installation path: /home/current-user/.local/share/umake/ide/idea

Follow hello_world project listed above.

Follow [ubuntu-make page][5] to change default installation and to install other IDEs.


Other
=====

Further installation details can be found here: https://www.jetbrains.com/help/idea/2016.1/installing-and-launching.html


  [1]:https://www.jetbrains.com/idea/
  [2]:https://www.wikiod.com/java/getting-started-with-java-language#Setting %PATH% and %JAVA_HOME% after installing on Windows
  [3]: http://i.stack.imgur.com/nJOLS.png
  [4]: http://i.stack.imgur.com/FxANu.png
  [5]: https://wiki.ubuntu.com/ubuntu-make

## Migrating from Eclipse
Intellij IDEA attempts to appeal to the wide Java fanbase which uses Eclipse for their development by allowing developers to migrate their Eclipse projects over to an IDEA structure with a few simple clicks!

First, start IDEA and click `Import Project` from the startup window:
[![Click "Import Project"][1]][1]
Then, select your Eclipse project using the explorer window
[![Select your Eclipse project][2]][2]

Intellij will prompt you for the model you are importing from, make sure `Eclipse` is selected before clicking `Next`
[![Just click "Next"][3]][3]

The next screen will show a confirmation of the path you want to import, simply click `Next`:
[![Click "Next"][4]][4]

Next, select the modules you want created. In the particular example project, only the `Alice` and the `BuggyRos` projects mattered when working in Eclipse
[![Select modules you want created][5]][5]

Finally, make sure the correct version of the JDK is selected before `Finish`ing
[![Select the correct JDK][6]][6]

And the Eclipse project has been fully migrated to Intellij! The project will still open in both IDEs, and will be fully functional in both


  [1]: http://i.stack.imgur.com/tbAti.png
  [2]: http://i.stack.imgur.com/zrvdq.png
  [3]: http://i.stack.imgur.com/fQs9w.png
  [4]: http://i.stack.imgur.com/8pZxN.png
  [5]: http://i.stack.imgur.com/PfiUl.png
  [6]: http://i.stack.imgur.com/DR9KR.png

