---
title: "CleverTap"
slug: "clevertap"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Quick hacks for the analytics and engagement SDK provided by CleverTap - Android

Get your CleverTap credentials from https://clevertap.com.

## Setting the debug level

In your custom application class, override the `onCreate()` method, add the line below:
```
CleverTapAPI.setDebugLevel(1);
```


## Get an instance of the SDK to record events


    CleverTapAPI cleverTap;
    try {
      cleverTap = CleverTapAPI.getInstance(getApplicationContext());
    } catch (CleverTapMetaDataNotFoundException e) {
      // thrown if you haven't specified your CleverTap Account ID or Token in your AndroidManifest.xml
    } catch (CleverTapPermissionsNotSatisfied e) {
      // thrown if you haven’t requested the required permissions in your AndroidManifest.xml
    }
    



