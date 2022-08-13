---
title: "Crash Reporting"
slug: "crash-reporting"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

**Crash Reporting** creates detailed reports of the errors in your app.  
Errors are grouped into clusters of similar stack traces and triaged by the severity of impact on your users. In addition to automatic reports, you can log custom events to help capture the steps leading up to a crash.

Crash Reporting is currently in beta release while we resolve some known issues on Android and iOS.

Official Documetantion
----------------------
https://firebase.google.com/docs/crash/

## Report the error in Android
Firebase Crash Reporting automatically generates reports for fatal errors (or uncaught exceptions).

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



## Setup Crash Reporting in Android
1. Complete the [Installation and setup part][1] to connect your app to Firebase.  
This will create the project in Firebase.

2. Add the dependency for Firebase CrashReporting to your module-level `build.gradle` file:  


    compile 'com.google.firebase:firebase-crash:9.4.0'


  [1]: https://www.wikiod.com/firebase/getting-started-with-firebase

