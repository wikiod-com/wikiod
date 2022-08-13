---
title: "Configuration Files"
slug: "configuration-files"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
- `String s = config.getString("path.to.string");`
- `int i = config.getInt("path.to.int");`
- `double d = config.getDouble("path.to.double");`
- `List<String> sl = config.getStringList("path.to.stringlist");`
- `List<Double> dl = config.getDoubleList("path.to.doublelist");`
- `List<Integer> il = config.getIntegerList("path.to.integerlist");`

The Bukkit configuration files are straight-forward Y.A.M.L (Yet Another Markup Language) files, and are implemented as so.

## Plugin Config.yml
You can have a config.yml file that loads directly from your jar file. It must be added to your project's folder, the same way as the plugin.yml file is.

In this file you have the default values for your configuration.

**Example config:**

    # This is an YML comment
    adminName: "Kerooker"
    moderators: ["Romario", "Pelé", "Cafú"]


The example config file must be added to the project folder.

To load the default configuration file to your plugin's folder, the following code must be added to your onEnable():

    saveDefaultConfig();

This will make your config.yml file from the project to be your plugin's configuration file, and will add it to your plugin's folder.

From there, you can access your config file from anywhere, by using your plugin instance:

    JavaPlugin plugin; // Your plugin instance
    FileConfiguration config = plugin.getConfig();  //Accessing the config file

From there, we can access anything that was set on the plugin's config.

Note: The default config file may have it's values changed, if the user wants to edit the config.yml file generated to the folder.

    String adminName = config.getString("adminName");
    List<String> moderators =  config.getStringList("moderators");

## Multiple Paths Section
What can happen in your config file is having a path to a variable that goes through multiple sections.

**Example Config**

    admins:
     first-tier: "Kerooker"
     second-tier: "Mordekaiser"
     third-tier: "Yesh4"

The name "Kerooker" is from section "first-tier", which is from section "admins". To access the inner paths of our file, we use a simple '.' as a way of saying that we want the next section. So, for us to access "Kerooker", we go:

    config.getString("admins.first-tier");

