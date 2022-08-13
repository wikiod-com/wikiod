---
title: "Getting started with sbt"
slug: "getting-started-with-sbt"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Install SBT on Linux
Full instructions can be [found here][1].

1. [Install the JDK][2].
    
2. Set the Java Environment variable.

       export JAVA_HOME=/usr/local/java/jdk1.8.0_102
       echo $JAVA_HOME
       /usr/local/java/jdk1.8.0_102
       export PATH=$PATH:$JAVA_HOME/bin/
       echo $PATH
       ...:/usr/local/java/jdk1.8.0_102/bin/
    
3. Install Scala.

       sudo wget http://www.scala-lang.org/files/archive/scala-2.11.8.deb
       sudo dpkg -i scala-2.11.8.deb
       sudo apt-get update
       sudo apt-get install scala

4. Install SBT.

       wget https://bintray.com/artifact/download/sbt/debian/sbt-0.13.9.deb
       sudo dpkg -i sbt-0.13.9.deb
       sudo apt-get update
       sudo apt-get install sbt

# RPM-based Linux Distributions

- Download SBT repository definitions and add it to YUM:
  ```
  curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
  ```
- Install SBT according to the definitions previously added to YUM:
  ```
  sudo yum install sbt
  ```

  [1]: http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html
  [2]: https://docs.oracle.com/javase/8/docs/technotes/guides/install/linux_jdk.html

## Install SBT on Windows
# Install

MSI installers can be [found here][1]. This is the latest [stable version][2]. Download and execute to install.

# Verify Installation

- Use the `WindowsKey + R`, type `cmd`.
- Alternatively, navigate to the `.sbt` (for example, in `C:\Users\Hopper`) and type `cmd` in the address bar.
- Type `sbt about` to get version information, verifying it is installed. You should see something like this:
  ```
  Java HotSpot(TM) 64-But Server VM warning: ignoring option MaxPermSize=256m; support was removed in 8.0
  [info] Set current project to root--sbt (in build file:/C:/Users/Hopper/.sbt/)
  [info] This is sbt 0.13.8
  ...
  ```

  [1]: https://dl.bintray.com/sbt/native-packages/sbt/
  [2]: https://dl.bintray.com/sbt/native-packages/sbt/:0.13.12/

## Install on Mac OSX
Full official instructions can be found [here][1].

# MacPorts

Install [MacPorts][2]. Then, in the terminal execute:

    port install sbt

# Homebrew

Install [Homebrew][3]. Then, in the terminal execute:

    brew install sbt

# Sources

Download sbt *All platforms* (tgz) installation from [SBT][4]. 

    sudo su
    cd /opt
    mkdir sbt
    cd sbt
    curl https://dl.bintray.com/sbt/native-packages/sbt/0.13.13/sbt-0.13.13.tgz -o sbt-0.13.13.tgz
   
Then, execute following

    tar zxf sbt-0.13.13.tgz
    ln -s sbt-0.13.13 latest

Inside your $HOME make sure to update ~/.profile - by adding following lines

    export SBT_HOME=/opt/sbt/latest
    export PATH=$PATH:$SBT_HOME/bin

# Verification

In the terminal execute:

    which sbt

You should expect output similar to:

    /opt/local/bin/sbt

If you get no output sbt is not installed.
    


  [1]: http://www.scala-sbt.org/0.13/docs/Setup.html
  [2]: https://www.macports.org/install.php
  [3]: http://brew.sh/
  [4]: http://www.scala-sbt.org/download.html

## Import SBT Project into Eclipse
This assumes you have installed both [Eclipse][1] and [SBT][2].

- Install the SBT plugin for Eclipse from the Eclipse marketplace.
- In the command line switch directory to the root directory of the project.
  
  ```$ cd ~/home/sample/project```

- Execute sbt, which will load the project.

  ```$ sbt```

- Compile the project to ensure dependencies are obtainable.

  ```> compile```

- Run the `eclipse` task:

  ```> eclipse```

- Go into Eclipse and select the menu option:

  ```File > New > Project From Existing Sources```

- In the wizard, navigate to your project directory and select it. Eclipse will handle the rest.


  [1]: https://www.wikiod.com/eclipse/getting-started-with-eclipse#Installation and Setup
  [2]: https://www.wikiod.com/sbt

