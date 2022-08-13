---
title: "Configuration"
slug: "configuration"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This topics contains aspects of using NuGet and its configurations.

## Changing path to the packages folder in Visual Studio solution
By default, NuGet restores packages into the **packages** folder in the solution root. This folder is shared between all solution projects. In some cases it is useful to change the location of the restored packages (for instance, to share them between several solutions).

Its can be achieved by creating the file **nuget.config** in the same folder where solution is located:

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
        <config>
            <add key="repositoryPath" value="../packages" />
        </config>
    </configuration>

**repositoryPath** setting points to the new packages location and automatically read by the NuGet during the restoring.

The config affects only one solution and it is enough just to put file near the **.sln**, no need to reference it inside solution itself.


