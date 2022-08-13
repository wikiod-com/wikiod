---
title: "Creating Your First Application With Cordova"
slug: "creating-your-first-application-with-cordova"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

In case running `cordova run android` fails. Make sure that your Android device is connected to your computer and run `adb devices` to make sure the Android Development Tools (ADT) can detect your device.

## Using the command-line tool
First you create a new Cordova project:

    cordova create HelloWorld my.application.identifier AppName

This will create a blank Cordova project 

 - in the *HelloWorld* folder
 - with identifier *my.application.identifier* (which should be unique for each application)
 - with name *AppName*. 

Next you add the desired platforms:

    cordova platform add android
    // and/or
    cordova platform add browser
    // and/or
    cordova platform add ios    // On macOS only
    // etc…

Build your application to generate executable file:

    cordova build                  // Build project for all platforms
    cordova build ios              // Build project only for iOS platform
    cordova build android          // Build project only for Android platform

Once built, you can run the app on one of the platforms you added:

    cordova run android --emulator  // Run Android app in emulator
    cordova run android --device    // Run Android app on physical connected device
    cordova run browser             // Will run the app in the browser

If you want to build the application for Eclipse, Xcode, Visual Strudio, etc:

    cordova prepare [platform_name] // Prepare copies of www folder and any plugins into the appropriate platform folder 

