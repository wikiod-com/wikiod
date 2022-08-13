---
title: "Getting started with akka"
slug: "getting-started-with-akka"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
 1. Install JDK 8 ([Windows][1], [Linux][1]) and set the path ([Windows][1]).
 2. Install Scala ([Linux][2]), For Windows visit http://www.scala-lang.org/download/ download and install binary distribution, set the environment variable for scala in PATH which is in `\scala\bin`.
 3. Installing [Typesafe activator][3] (It contains Scala, Akka, Play, SBT) + project scaffolding and templates. For quick start download the `mini-package`.
 4. Extract the Typesafe activator and set the PATH to `activator-x.x.xx-minimal\bin` (It includes the bash and bat scripts to run the activator).
 5. Time to create a sample project and import into your favorite IDE.


----------

 - Type `activator new` in cmd/terminal.

[![enter image description here][4]][4]
 - You can choose `4` because [Hello World example][5] is based on Scala.
 - Import the project to your favorite IDE and start with the [Hello World example][5].
 - Done !.


  [1]: https://www.wikiod.com/java/installing-java-standard-edition
  [2]: https://www.wikiod.com/scala/setting-up-scala
  [3]: https://www.lightbend.com/activator/download
  [4]: http://i.stack.imgur.com/toOhR.png
  [5]: https://www.wikiod.com/akka/hello-world

 1. Download akka-2.0.zip distribution of Akka from http://akka.io/downloads/
 2. Unzip akka-2.0.zip in any directory. (Example - /home/USERNAME/tools/akka-2.0) You would like to have Akka installed.
 3. Set the `AKKA_HOME`
 4. For Linux.

        # First got to the installed location
        cd /home/USERNAME/tools/akka-2.0
        
        # Export the location as AKKA_HOME
        export AKKA_HOME=`pwd`
        
        # Check if PATH is Exported.    
        echo $AKKA_HOME
        /home/USERNAME/tools/akka-2.0

 5. For Windows

        # First got to the installed location        
        C:\USERNAME\akka> cd akka-2.0
        
        # Set the location as AKKA_HOME
        C:\USERNAME\akka\akka-2.0> set AKKA_HOME=%cd%
        
        # Check if PATH is Exported. 
        C:\USERNAME\akka\akka\akka-2.0> echo %AKKA_HOME%
        C:\USERNAME\akka\akka-2.0



