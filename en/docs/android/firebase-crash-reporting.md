---
title: "Firebase Crash Reporting"
slug: "firebase-crash-reporting"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## How to report an error
[Firebase Crash Reporting][1] automatically generates reports for fatal errors (or uncaught exceptions).

You can create your custom report using:

    FirebaseCrash.report(new Exception("My first Android non-fatal error"));

You can check in the log when FirebaseCrash initialized the module:

>07–20 08:57:24.442 D/FirebaseCrashApiImpl: **FirebaseCrash reporting API initialized**
07–20 08:57:24.442 I/FirebaseCrash: **FirebaseCrash reporting initialize**d com.google.firebase.crash.internal.zzg@3333d325
07–20 08:57:24.442 D/FirebaseApp: **Initialized class com.google.firebase.crash.FirebaseCrash.**

And then when it sent the exception:

>07–20 08:57:47.052 D/FirebaseCrashApiImpl: **throwable java.lang.Exception: My first Android non-fatal error**
07–20 08:58:18.822 D/FirebaseCrashSenderServiceImpl: **Response code: 200**
07–20 08:58:18.822 D/FirebaseCrashSenderServiceImpl: **Report sent**

You can add custom logs to your report with

    FirebaseCrash.log("Activity created");


  [1]: https://www.wikiod.com/android/firebase#How to add Firebase Crash Reporting to your app

## How to add Firebase Crash Reporting to your app
In order to add _Firebase Crash Reporting_ to your app, perform the following steps:

* Create an app on the _Firebase Console_ [here][1].
* Copy the `google-services.json` file from your project into your in `app/` directory.
* Add the following rules to your root-level _build.gradle_ file in order to include the `google-services` plugin:

      buildscript {
          // ...
          dependencies {
              // ...
              classpath 'com.google.gms:google-services:3.0.0'
          }
      }

* In your module Gradle file, add the `apply plugin` line at the bottom of the file to enable the Gradle plugin:

      apply plugin: 'com.google.gms.google-services'

* Add the dependency for _Crash Reporting_ to your app-level _build.gradle_ file:

      compile 'com.google.firebase:firebase-crash:10.2.1'

* You can then fire a custom exception from your application by using the following line:

      FirebaseCrash.report(new Exception("Non Fatal Error logging"));

  All your fatal exceptions will be reported to your _Firebase Console_.

* If you want to add custom logs to a console, you can use the following code:

      FirebaseCrash.log("Level 2 completed.");

For more information, please visit:
- [Official documentation][2]
- [Stack Overflow dedicated topic][3]


  [1]: https://console.firebase.google.com/
  [2]: https://firebase.google.com/docs/crash/android
  [3]: https://www.wikiod.com/firebase/crash-reporting

