---
title: "NMS"
slug: "nms"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

NMS, also known as **N**et.**M**inecraft.**S**erver is the package which contains core Minecraft server code. Classes in this package were made by Mojang (not Bukkit) and are therefore mostly obfuscated and not meant to be used or altered. However, interacting with the Minecraft server code at this level allows you to modify almost every aspect of it. This is significant because there are numerous modifications that Bukkit does not support.

The Bukkit API is a wrapper or abstraction layer for NMS that allows plugin developers to interact with the server without worrying about changes made to the internal codebase.


Use of NMS code is discouraged as it breaks often between Minecraft version changes and cannot be supported by Bukkit or Spigot as they do not create, own, or maintain it.

## Accessing the Current Minecraft Version
One of the most critical parts of dealing with NMS code is being able to support mulitple Minecraft versions. There are numerous ways to do this, but a simple solution is to use this code to store the version as a public static field:

    public static final String NMS_VERSION = Bukkit.getServer().getClass().getPackage().getName().substring(23);


This code snippet works by taking the CraftServer class:

`org.bukkit.craftbukkit.VERSION.CraftServer.class`

Getting its package:

`org.bukkit.craftbukkit.VERSION`

And taking the substring of the package name starting at the index 23 which will always be after 'org.bukkit.craftbukkit.' (which has a length of 23 characters). Resulting in the final VERSION string:

`VERSION`


There are a number of reasons why it is so important to be able to access the current Minecraft version. Mostly because any accessing of a class on a server running a different Minecraft version than what the plugin was coding with will throw an Error.


Here is an example that demonstrates how to solve that issue by using the `NMS_VERSION` field to retrieve an instance of CraftPlayer (which is a NMS class) on any Minecraft version.

    /**
     * Invokes the getHandle() method on the player's CraftPlayer instance to
     * retrieve the EntityPlayer representation of the player as an Object to
     * avoid package version change issues
     * 
     * @param player
     *            the player to cast
     * @return the NMS EnityPlayer representation of the player
     */
    public static Object getCraftPlayer(Player player) {
        try {
            return Class.forName("org.bukkit.craftbukkit." + NMS_VERSION + ".entity.CraftPlayer")
                    .getMethod("getHandle")
                    .invoke(player);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException | ClassNotFoundException e) {
            throw new Error(e);
        }
    }


The resulting object can then be manipulated using reflection to perform NMS based tasks without worrying about trying to access the wrong version of the class.

Even this method is not foolproof however, as NMS field and method names change easily, so the only thing you're guaranteeing by doing this is that your code wont definitely break each time Minecraft updates.

## Getting a Player's Ping
One very simple thing that you might want to do with NMS that Bukkit doesn't support is get the player's ping. This can be done like this:

    /**
     * Gets the players ping by using NMS to access the internal 'ping' field in
     * EntityPlayer
     * 
     * @param player
     *            the player whose ping to get
     * @return the player's ping
     */
    public static int getPing(Player player) {
        EntityPlayer entityPlayer = ((CraftPlayer) player).getHandle();
        return entityPlayer.ping;
    }


If you're using a method like getCraftPlayer(Player) which returns an instance of the Player's corresponding CraftPlayer instance as an Object. You can access the data without importing the version dependent classes by using reflection like this:

    /**
     * Gets the player's ping using reflection to avoid breaking on a Minecraft
     * update
     * 
     * @param player
     *            the player whose ping to get
     * @return the player's ping
     */
    public static int getPing(Player player) {
        try {
            Object craftPlayer = getCraftPlayer(player);
            return (int) craftPlayer.getClass().getField("ping").get(craftPlayer);
        } catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
            throw new Error(e);
        }
    }

