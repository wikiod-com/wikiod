---
title: "Android Studio optimization"
slug: "android-studio-optimization"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Parameters
| Parameter | Detail |
| ------ | ------ |
| Xms   | Initial memory allocate  |
| Xmx   | Max memory allocate   |

## Customize the VM Option
You can override the default `vmoptions` with your own personal settings by choosing **Help > Edit Custom VM Options** from the Android Studio toolbar. This will create a local copy which you are free to edit.

Alternatively, you can edit the default `vmoptions` directly using the paths given below. Note that this method is not recommended, and your changes may be overwritten when updating Android Studio.

Windows:
------------

    %USERPROFILE%\.{FOLDER_NAME}\studio.exe.vmoptions and/or %USERPROFILE%\.{FOLDER_NAME}\studio64.exe.vmoptions
    %USERPROFILE%\.{FOLDER_NAME}\idea.properties

Mac:
-------------

    ~/Library/Preferences/{FOLDER_NAME}/studio.vmoptions
    ~/Library/Preferences/{FOLDER_NAME}/idea.properties

Linux:
------------

    ~/.{FOLDER_NAME}/studio.vmoptions 
    ~/.{FOLDER_NAME}/studio64.vmoptions
    ~/.{FOLDER_NAME}/idea.properties

Default setting for Android Studio 64-bit
------

    -Xms128m
    -Xmx750m
    -XX:MaxPermSize=350m
    -XX:ReservedCodeCacheSize=96m
    -ea
    -Dsun.io.useCanonCaches=false
    -Djava.net.preferIPv4Stack=true
    -Djna.nosys=true
    -Djna.boot.library.path=
    
    -Djna.debug_load=true
    -Djna.debug_load.jna=true
    -Djsse.enableSNIExtension=false
    -XX:+UseCodeCacheFlushing
    -XX:+UseConcMarkSweepGC
    -XX:SoftRefLRUPolicyMSPerMB=50
    -Didea.platform.prefix=AndroidStudio
    -Didea.paths.selector=AndroidStudio

Optimized Setting
-----------

    -Xms1024m
    -Xmx4096m
    -XX:MaxPermSize=1024m
    -XX:ReservedCodeCacheSize=256m
    -ea
    -Dsun.io.useCanonCaches=false
    -Djava.net.preferIPv4Stack=true
    -Djna.nosys=true
    -Djna.boot.library.path=
    
    -Djna.debug_load=true
    -Djna.debug_load.jna=true
    -Djsse.enableSNIExtension=false
    -XX:+UseCodeCacheFlushing
    -XX:+UseConcMarkSweepGC
    -XX:SoftRefLRUPolicyMSPerMB=50
    -Didea.platform.prefix=AndroidStudio
    -Didea.paths.selector=AndroidStudio



