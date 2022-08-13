---
title: "Getting started with adb"
slug: "getting-started-with-adb"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
`adb` is a command line tool for communicating with an emulator instance or connected device. It allows for installing and debugging apps, transferring files, as well as a variety of other interactions with the connected emulator or device. The ADB system consists of a *client*, which sends commands from the host computer, a *daemon*, which runs on the connected device and executes commands received from the client, and a *server*, which runs on the host computer and manages communications between the client and daemon.

**Official Documentation**

https://developer.android.com/studio/command-line/adb.html

## Installation or Setup
Specific to Windows System and android Phone:

Requirements:

1) USB Cable
2) Android Device
3) Android Driver Software

Basically after connecting USB cable PC detects the Android Device and it will automatically search for the required Drivers for that Android Device. If that drivers are not found then you have to install manually.

Manual Installation:

1) First install Android SDK in your PC(Windows)

2) After installing in Android SDK tools Right click on the SDK Manager and select "Run as Administrator"

3) In the SDK Manager select "Extras->Google USB Driver". Enable the checkbox and click "Install 1 Package"

4) When the Google USB driver is installed, plug in your device. 
Warning: The driver won't install automatically. We will do it manually in the next      steps.

5) Open the System Properties dialog (press Win+Break on the keyboard or locate "Computer" in Start Menu, right-click on it and select "Properties".

6) Click on the "Device Manager" link.

7) In the Device Manager locate your Android device. Then right-click on it and select "Update Driver Software".

8) Select "Browse my computer for driver software".

9) Select "Let me pick from a list of device drivers on my computer".

10) Select "Show All Devices".

11) Press the "Have Disk" button.

12) Enter the path to the Google USB driver. Normally it is located in the following directory:C:\Program Files (x86)\Android\android-sdk\extras\google\usb_driver

13) Select "Android ADB Interface" from the list of device types.

14) Confirm the installation of the driver by pressing "Yes".

15) Confirm the installation again by pressing "Install".

16) When the installation is done, press "Close".

17) 



