---
title: "Configuring Eclipse"
slug: "configuring-eclipse"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Increasing maximum heap memory for Eclipse
To increase the maximum amount of heap memory used Eclipse, edit the `eclipse.ini` file located in the Eclipse installation directory. 

This file specifies options for the startup of Eclipse, such as which JVM to use, and the options for the JVM. Specifically, you need to edit the value of the `-Xmx` JVM option (or create one if it does not exist).

Below is an example configuration that sets a maximum heap memory of 1 GB (1024m). The relevant line is `-Xmx1024m`- this would replace the existing `-Xmx*` line in your confiugration:

    -startup
    plugins/org.eclipse.equinox.launcher_1.3.200.v20160318-1642.jar
    --launcher.library
    C:/Users/user1/.p2/pool/plugins/org.eclipse.equinox.launcher.win32.win32.x86_64_1.1.400.v20160518-1444
    -product
    org.eclipse.epp.package.java.product
    --launcher.defaultAction
    openFile
    -showsplash
    org.eclipse.platform
    --launcher.appendVmargs
    -vmargs
    -Xms256m
    -Xmx1024m



## Specifying the JVM
A common issue that users of Eclipse encounter is related to the default system JVM. 

A typical situation is a 64 bit Windows which has both 32 and 64 bit versions of Java installed, and a 32 bit Eclipse. If the 64 bit version of Java is the system default, when Eclipse is launched an error dialog is shown.

Specifying the JVM explicitly in `eclipse.ini` will resolve this. The `-vm` entry should be added directly above the `-vmargs` section.

The example below shows how to use a 32 bit JVM on a 64 bit Windows:

    -startup
    plugins/org.eclipse.equinox.launcher_1.3.200.v20160318-1642.jar
    ...
    -vm
    C:/Program Files (x86)/Java/jdk1.7.0_71/bin/javaw.exe
    -vmargs
    -Xms256m
    -Xmx1024m



## How to configure the font size of views in Eclipse on Linux
Eclipse does not give you the possibility to change the font size of the views like 'Project Explorer' or 'Servers', which looks ugly on Linux since Eclipse uses the default (desktop) font size. But you can edit specific configuration files to get the proper font sizes.

To fix this annoying font size, go to   
`/[YOUR_INST_DIR]/eclipse/plugins/org.eclipse.ui.themes_[LATEST_INSTALLATION]/css` 
and add this content...

    .MPart Tree{
      font-family: Sans;
      font-size: 8px;
    }

 

to the bottom of the following files:  

`e4_classic_winxp.css`  
`e4_classic_win7.css`

**BEFORE CHANGE**  

[![projectexplorer1][1]][1]  
[![server1][2]][2]

**AFTER CHANGE**  

[![projectexplorer2][3]][3]
[![servers2][4]][4]


  [1]: https://i.stack.imgur.com/LlLMk.png
  [2]: https://i.stack.imgur.com/KhF6b.png
  [3]: https://i.stack.imgur.com/KEHk0.png
  [4]: https://i.stack.imgur.com/MzgJl.png

