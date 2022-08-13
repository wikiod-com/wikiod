---
title: "Creating first Spigot plugin"
slug: "creating-first-spigot-plugin"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## First Plugin in Eclipse
Prerequisite
============

This guide assumes you have already used [BuildTools][1] and have run the [Spigot server][2] at least once. It also assumes that you have the Spigot-API jar file which we will use. 

**1) Start Eclipse**; you may change the workspace location if desired.

**2) Create a new Project** 
    
   1. Set the project name to whatever you wish. Here, we chose MyFirstPlugin. 
   2. Click next.
   3. Select Add External JARs under the Libraries tab. In the JAR Selection dialogue box, select the spigot-api-shaded jar file, which can be found in Spigot/Spigot-API/target/ inside your BuildTools folder. 
   4. Select Finish

**3) Add a new package** 

Right click on **src** and click on **New > Package**. You may use any namespace convention you wish, just be consistent. (e.g: com.google.android).

**4) Create a new class**

1. Right-click on the newly created package and select **New > Class**. 
2. Give it any name; often the same name as the project. Inside the editor, the newly created Java class will open. The code should look somewhat like this:

        package yourpackage;
        public class MyFirstPlugin {
        }

**5) Modify class declaration**

1. Your class must extend from JavaPlugin. Eclipse will produce an error as it does not know what JavaPlugin is. If you have successfully imported the Spigot-API, you will be able to import JavaPlugin by adding the import statement. You do not need to manually type that line, simply click the error and select the appropriate action. Your code should now look like:

        package yourpackage;
        import org.bukkit.plugin.java.JavaPlugin;

        public class MyFirstPlugin extends JavaPlugin {

        }
 
**6) Implement the necessary methods**

The JavaPlugin class has some abstract methods which must be implemented by your plugin. Hence, add the onEnable and onDisable functions which will be triggered when the plugin is disabled or enabled in the console. You can leave these blank for now. You are also required to write `@Override` above the method.

Note: You do not need to add a getLogger when your plugin is enabled or disabled, Bukkit already does that for you.


    package com.meeku.tutorialPlugin;
    import org.bukkit.plugin.java.JavaPlugin;

    public class MyFirstPlugin extends JavaPlugin {
        // Fired when plugin is enabled
        @Override
        public void onEnable() {
        }
        // Fired when plugin is disabled
        @Override
        public void onDisable() {
    
        }
    }
 
**7) Create plugin.yml file**

Right click the project and create a file **New > File**. Name it **plugin.yml**. Paste in the following:

    name: MyFirstPlugin
    main: yourpackage.MyFirstPlugin
    version: 1.0
    commands:

**8) Export**
 
Since there are no errors, we can export this project as a JAR. Right-click on the project name, select Export. In the consequential dialogue box, select JAR file. Click Next. You can uncheck the classpath and project include and change the export destination to your plugins folder 

**9) Running**

Start the server and you should see that your plugin was enabled.


  [1]: https://www.wikiod.com/minecraft/installing-a-spigot-server#BuildTools
  [2]: https://www.wikiod.com/minecraft/installing-a-spigot-server#Spigot Installation

