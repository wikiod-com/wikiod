---
title: "Installing a Spigot server"
slug: "installing-a-spigot-server"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## BuildTools
What is it?
===========

BuildTools.jar is a solution to building Bukkit, CraftBukkit, Spigot, and the Spigot-API. All of which is done on your computer! A few prerequisite programs are necessary, but the instructions below will guide you through everything you need to do.

Prerequisites
=============

There are two applications necessary to use BuildTools: Git and Java.

Windows
============

Git
---

In order for BuildTools to run on Windows, you will need to install Git. For Windows it is distributed via git-scm, which can be downloaded [here][1]. Install it where you like, it will provide git bash, which will be used to run the BuildTools jar. Just keep hitting next when running the installer.

Java
----

 Download JRE 8 from [here][2] and install. Just keep hitting next when running the installer.

Linux
=====

Both git and Java, as well as util commands, can be installed using a single command via your package manager.

Debian/Ubuntu: `sudo apt-get install git openjdk-7-jre-headless tar`

CentOS/RHEL: `sudo dnf install git java-1.7.0-openjdk-devel tar`

Arch: `pacman -S jdk8-openjdk git`

Mac
===

Git can be downloaded from: http://sourceforge.net/projects/git-osx-installer/files/

Java may need to be updated from the Apple distributed version, and even if previously updated, may need to be linked for shell use.
Please follow steps found here: https://gist.github.com/johan/10590467

Running BuildTools
==================

 1. Download BuildTools.jar from https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar.

2. Open your terminal if you are on Linux, or git bash on Windows.
    1. Git bash can be found on the desktop or in the Start menu under the name "git bash". It's also possible to open it by right-clicking on anything, as it is now an item in your context menu.

3. Navigate to where you downloaded BuildTools.jar, or use the command line way to download the jar to your current directory.
    1. On Windows, you can either use the cd command to change directories, or you can right click the blank space of the folder where BuildTools.jar is (DO NOT click BuildTools.jar itself) and click "git bash", which will open it in your current directory.

4. Run BuildTools.jar from the terminal (Do not double-click BuildTools.jar) by doing the following:
    1. On Linux run git config --global --unset core.autocrlf, then run java -jar BuildTools.jar in bash or another appropriate shell.
    2. On Windows run the below command inside the git bash window that opened:
java -jar BuildTools.jar
Please be aware that it is required that you have BuildTools #35 or later, older versions will not work.
    3. On Mac run the below commands,
export MAVEN_OPTS="-Xmx2G"
java -Xmx2G -jar BuildTools.jar

5. Wait as it builds your jars. In a few minutes you should have freshly compiled jars!

6. You can find CraftBukkit and Spigot in the same directory you ran the the BuildTools.jar in (craftbukkit-1.10.jar and spigot-1.10.jar). You can find Spigot-API in \Spigot\Spigot-API\target\ (spigot-api-1.10-R0.1-SNAPSHOT.jar).


  [1]: http://msysgit.github.io/
  [2]: http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html

## Spigot Installation
Windows
=======
1. Get spigot.jar using BuildTools or from [here][1].
2. Paste the following text into a text document. Save it as start.bat in the same directory as spigot.jar:
You will need to rename your jar to spigot.jar, or modify the file in the bat file to point to the correct file.
nb: Windows (by default) will hide the .jar extension of the file.

        @echo off
        java -Xmx1G -jar spigot.jar
        pause
3. Double click the batch file.

Linux
=====

1. Get spigot.jar using BuildTools or from [here][1].
2. Create a new startup script (start.sh) in the directory to launch the the JAR:
        #!/bin/sh

        java -Xmx1G -jar spigot.jar
3. Open your terminal and execute the following in the directory:
        chmod +x start.sh
4. Run your start up script:

        ./start.sh

Mac
===
1. Get spigot.jar using BuildTools or from [here][1].
2. Create a new startup script (start.command) to launch the JAR:
        #!/bin/sh

        cd "$( dirname "$0" )"
        java -Xmx1G -jar spigot.jar

3. Open Terminal and type into it: (Don't hit enter!)

        chmod a+x
4. Drag your startup script file into the Terminal window. (Be sure to put a space between chmod a+x and your startup script!)
5. Double click your startup script.

  [1]: https://getbukkit.org/spigot

