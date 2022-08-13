---
title: "Scala"
slug: "scala"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

How to implement Bukkit plugins in the Scala programming language

## Project setup (Scala Eclipse)
Creating a project in scala is very similar to creating one in java. Here is what the entry class should look like:

    package com.example.myplugin; //{$TopLevelDomain}.{$Domain}.{$PluginName}

    import org.bukkit.plugin.java.JavaPlugin
    import org.bukkit.command.CommandSender
    import org.bukkit.command.Command

    class PluginName extends JavaPlugin {

      override def onEnable() {

      }

      override def onDisable() {

      }

      override def onCommand(sender: CommandSender, cmd: Command, label: String, args: Array[String]): Boolean = {

        false
      }

    }

First, make sure you have installed the latest Scala version located here: https://www.scala-lang.org/download/

Next, you'll want to download Scala Eclipse, available here: http://scala-ide.org/
and extract the download to a folder of your choice.

Once these are both installed, simply open Scala Eclipse.

Lastly, in order for your plugin to work - you need to have some sort of runtime plugin to load the scala library for you, I use this one: https://dev.bukkit.org/projects/scala-loader (place this jar in your plugins folder just like any other plugin)

From here on out, the process is almost identical to java:
1. Press `Alt+Shift+N` -> click `Scala Project`
2. Right-click on your project - click `Properties`
3. Click `Java Build Path`, then click on the `Libraries` tab
4. Click `Add External Jars`, and select your spigot-api jar file
5. Click `Apply` and then `OK`

For the project setup, you'll want to create a package so:

Right-click on project -> `New` -> `Package`

Name it how you'd like, typically: `com.yourdomain.pluginname`

Inside of this package, create a Scala Class and name it how you'd like, typically: `PluginName`

Make the class `extends JavaPlugin` and override the provided functions for a basic setup as shown above.

Lastly, Right-Click on the folder called "src" and select New File. Name the file plugin.yml (NOT the name of your plugin, but explicitly plugin.yml) and open it.

A basic implementation should look like this:

    name: PluginName
    main: com.example.pluginname.PluginName
    version: 0.1

And there you have it! After you're done writing your plugin, click `File` -> `Export` -> `Java` -> `Jar file` -> Select your project and specify your server's pluginfolder as the destination -> click `Finish`

Typically you can simply reload your server to see the changes after export, however **some plugins will break on reload, so be careful!** I advise always **restarting** the server unless you know that reloading will not break other plugins.



