---
title: "Getting started with bukkit"
slug: "getting-started-with-bukkit"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a Plugin
# Prerequisites
 - JDK 7 or Higher (Recommended: JDK 8+)

<br>

# Adding Bukkit as a Dependency
The simplest method to add the Bukkit API to your project is to download the Bukkit.jar directly from the [Spigot Repository](https://hub.spigotmc.org/nexus/content/repositories/snapshots/org/bukkit/bukkit/) and add it to your project's classpath. Legacy versions of Bukkit can be found at the [Bukkit Repository](http://repo.bukkit.org/content/groups/public/org/bukkit/bukkit/).


The other is to add it as a Maven dependency, by adding the following lines to your `pom.xml`:

<!-- language: lang-xml -->
    <repositories>
        <repository>
            <id>spigot-repo</id>
            <url>https://hub.spigotmc.org/nexus/content/repositories/snapshots/</url>
        </repository>
    </repositories>
    <dependencies>
        <!--Bukkit API-->
        <dependency>
            <groupId>org.bukkit</groupId>
            <artifactId>bukkit</artifactId>
            <version>{VERSION}</version>
            <scope>provided</scope>
        </dependency>
    </dependencies>

<br>

# Main Class

The plugin's main class is the entry point for Bukkit to load an interact with your plugin. It is a class that extends `JavaPlugin` and only one instance of it should be created by your plugin. By convention it is good to give this class the same name as your plugin. 


Here is an example of a main plugin class for the plugin "MyPlugin":


<!-- language: lang-java -->
    package com.example.myplugin; //{$TopLevelDomain}.{$Domain}.{$PluginName};
    
    import org.bukkit.plugin.java.JavaPlugin;
    
    public final class MyPlugin extends JavaPlugin {
    
        @Override
        public void onEnable() {
            //Called when the plugin is enabled
            getLogger().info("onEnable has been invoked!");
        }
    
        @Override
        public void onDisable() {
            //Called when the plugin is disabled
            getLogger().info("onDisable has been invoked!");
        }

    }

To access your plugin instance from another class, you'll need to store the instance of your MyPlugin class created by Bukkit so that is accessible from outside of the class.

<!-- language: lang-java -->
    public class MyPlugin extends JavaPlugin {

        private static MyPlugin instance; //Effectively final variable containing your plugin's instance

        public MyPlugin(){
            if(MyPlugin.instance != null) { //Unnecessary check but ensures your plugin is only initialized once.
                throw new Error("Plugin already initialized!");
            }

            MyPlugin.instance = this; //A plugin's constructor should only be called once
        }

        public static MyPlugin getInstance(){ //Get's your plugin's instance
            return instance;
        }

        //your other code...
    }

Then, to access your main class from another class, simply use `MyPlugin.getInstance()`

<!-- language: lang-java -->
    public class MyOtherClass {

        public void doSomethingWithMainClass(){
            MyPlugin.getInstance().getLogger().info("We just used MyPlugin");
        }

    }

<br>

# Creating a plugin.yml

The plugin.yml file goes in the root of your final jar file and provides essential information to Bukkit to load your plugin. The most simple plugin.yml looks like this

<!-- language: lang-yaml -->
    name: {$PluginName}               //The name of the plugin
    main: {$PackageName}.{$MainClass} //The fully qualified name of the main class.
    version: {$Version}               //The plugin's version


For example with the above MyPlugin class

<!-- language: lang-yaml -->
    name: MyPlugin
    main: com.example.myplugin.MyPlugin
    version: 1.0

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
    4. If you need older version, you can specify the version using --rev argument to BuildTools, for example for 1.8.8: java -jar BuildTools.jar --rev 1.8.8


5. Wait as it builds your jars. In a few minutes you should have freshly compiled jars!

6. You can find CraftBukkit and Spigot in the same directory you ran the the BuildTools.jar in (for minecraft version 1.10, they would be craftbukkit-1.10.jar and spigot-1.10.jar). You can find Spigot-API in \Spigot\Spigot-API\target\ (for minecraft version 1.10, it would be spigot-api-1.10-R0.1-SNAPSHOT.jar).


  [1]: http://msysgit.github.io/
  [2]: http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html

## Create a test server on Windows


