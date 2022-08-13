---
title: "Plugin Commands"
slug: "plugin-commands"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Hello Command
In the code below you can see how to add a command to your plugin.

MainClass.java
=======

    package yourpackage;

    import org.bukkit.command.Command;
    import org.bukkit.command.CommandSender;
    import org.bukkit.plugin.java.JavaPlugin;

    public class MainClass extends JavaPlugin {

    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (command.getName().equalsIgnoreCase("hello")) {
            sender.sendMessage("Hey!");
        }
        return false;
    }
    }


Plugin.yml
==========
    name: HelloCommand
    main: yourpackage.MainClass
    version: 1.0
    commands:
      hello:
        description: Hello

