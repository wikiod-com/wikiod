---
title: "Versions"
slug: "versions"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Getting Version on Runtime
    @Override
    public void onEnable() {
        String version = Bukkit.getBukkitVersion();    //The string version of this bukkit server
    }

## Accessing NMS across implementation versions
    // gets the Class objects from the net.mminecraft.server package with the given name
    public Class<?> getNmsClass(String name) throws ClassNotFoundException {
        // explode the Server interface implementation's package name into its components
        String[] packageArray = Bukkit.getServer().getClass().getPackage().getName().split("\\.");

        // pick out the component representing the package version if it's present
        String packageVersion = array.length == 4 ? array[3] + "." : "";

        // construct the qualified class name from the obtained package version
        String qualName = "net.minecraft.server." + packageVersion + name;

        // simple call to get the Class object
        return Class.forName(qualName);
    }

