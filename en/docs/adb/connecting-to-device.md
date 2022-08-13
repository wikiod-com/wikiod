---
title: "Connecting to device"
slug: "connecting-to-device"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Finding devices connected to your PC
Enable USB Debugging on your device and from command line type `adb devices`. If everything is OK, the response should be:  

>List of devices attached  
1234567890        device  

Where `1234567890` is the device's id.  
If multiple devices are connected, you should see all of them:  

>List of devices attached  
1234567890       device  
2222222222       device  
...

When connecting a device for the first time, you'll get a pop-up window on your device, asking you to approve the connection.


