---
title: "Getting started with playframework"
slug: "getting-started-with-playframework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with Play 2.4.x/2.5.x - Windows, Java
Installations
=============
Download and install:
1. Java 8 - download the relevant installation from <a href="http://www.oracle.com/technetwork/java/javase/downloads/index.html">Oracle site</a>.
2. Activator - download zip from <a href="https://www.playframework.com/download">www.playframework.com/download</a> and extract files to the target Play folder, for example to:

       c:\Play-2.4.2\activator-dist-1.3.5
3. sbt - download from <a href="http://www.scala-sbt.org/">www.scala-sbt.org</a>.

Define environment variables:
1. <b>JAVA_HOME</b>, for example:

       c:\Program Files\Java\jdk1.8.0_45

2. <b>PLAY_HOME</b>, for example:

       c:\Play-2.4.2\activator-dist-1.3.5;
3. <b>SBT_HOME</b> for example:

       c:\Program Files (x86)\sbt\bin;  
Add path to all three installed programs to the path variables:

     %JAVA_HOME%\bin;%PLAY_HOME%;%SBT_HOME%;

Play 2.5 installation fix
-------------------------
Installation of Play 2.5.3 (the last 2.5 stable release) comes with a minor problem. 
To fix it:
1. Edit the file *activator-dist-1.3.10\bin\activator.bat* and add the "%" character at the end of line 55. The proper line should be like this: 
*set SBT_HOME=%BIN_DIRECTORY%*
2. Create sub-directory *conf* under the activator root directory *activator-dist-1.3.10*.
3. Create in the *conf* directory an empty file named *sbtconfig.txt*.

Creating a new application with CLI
====================================
Start the *cmd* from the directory, where a new application should be created.
The shortest way to create a new application via CLI is to provide an application name and template as CLI arguments:

      activator new my-play-app play-java
It is possible to run just:

      activator new
In this case you will be prompted to select the desired template and an application name.

For Play 2.4 add manually to *project/plugins.sbt*:

    // Use the Play sbt plugin for Play projects
    addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.x")
Be sure to replace 2.4.x here by the exact version you want to use.
Play 2.5 generates this line automatically.

Make sure that the proper **sbt** version is mentioned in project/build.properties. It should match to **sbt** version, installed on your machine. For example, for Play2.4.x it should be:

    sbt.version=0.13.8

That's it, a new application now may be started:

      cd my-play-app
      activator run
After a while the server will start and the following prompt should appear on the console:
 
      [info] p.c.s.NettyServer - Listening for HTTP on /0:0:0:0:0:0:0:0:9000
      (Server started, use Ctrl+D to stop and go back to the console...)
The server by default is listening on port 9000. You can request it from a browser by the URL *http://localhost:9000*. You will get something like this:

[![enter image description here][1]][1]

Running activator on a different port
-------------------------------------
By default the activator runs an application on port 9000 for http or 443 for https.
To run an application on the different port (http):
     
    activator "run 9005"


  [1]: http://i.stack.imgur.com/D0dgS.png


## Play 1 Installation
# Prerequisites
To run the Play framework, you need Java 6 or later. If you wish to build Play from source, you will need the [Git source control client][1] to fetch the source code and [Ant][2] to build it.

Be sure to have Java in the current path (enter `java --version` to check)

Play will use the default Java or the one available at the **$JAVA_HOME** path if defined.

The **play** command line utility uses Python. So it should work out of the box on any UNIX system (however it requires at least Python 2.5).

# Installation from the binary package
## Generic instructions
In general, the installation instructions are as follows.

 1. Install Java.
 2. Download the [latest Play binary package][3] and extract the archive.
 3. Add the ‘play’ command to your system path and make sure it is executable.

## Mac OS X
Java is built-in, or installed automatically, so you can skip the first step.

 1. Download the latest Play binary package and extract it in `/Applications`.
 2. Edit `/etc/paths` and add the line `/Applications/play-1.2.5` (for example).

An alternative on OS X is:

 1. Install [HomeBrew][4]
 2. Run `brew install play`

## Linux
To install Java, make sure to use either the Sun-JDK or OpenJDK (and not gcj which is the default Java command on many Linux distros)

## Windows
To install Java, just download and install the latest JDK package. You do not need to install Python separately, because a Python runtime is bundled with the framework.

  [1]: https://git-scm.com/
  [2]: http://ant.apache.org/
  [3]: http://download.playframework.com/
  [4]: http://mxcl.github.com/homebrew/

## Installing through `sbt`
If you already have `sbt` installed I find it easier to create a minimal Play project without `activator`. Here's how.

    # create a new folder
    mkdir myNewProject
    # launch sbt
    sbt

When previous steps are completed, edit `build.sbt` and add the following lines

    name := """myProjectName"""
    
    version := "1.0-SNAPSHOT"
    
    offline := true
    
    lazy val root = (project in file(".")).enablePlugins(PlayScala)
    scalaVersion := "2.11.6"
    # add required dependencies here .. below a list of dependencies I use
    libraryDependencies ++= Seq(
      jdbc,
      cache,
      ws,
      filters,
      specs2 % Test,
      "com.github.nscala-time" %% "nscala-time" % "2.0.0",
      "javax.ws.rs" % "jsr311-api" % "1.0",
      "commons-io" % "commons-io" % "2.3",
      "org.asynchttpclient" % "async-http-client" % "2.0.4",
      cache
    )
    
    
    resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
    
    resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
    
    resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/"


Finally, create a folder `project` and inside create a file `build.properties` with the reference to the version of Play you would like to use

     addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3")


That's it! Your project is ready. You can launch it with `sbt`. From within `sbt` you have access to the same commands as with `activator`.

