---
title: "Strict Mode Policy  A tool to catch the bug in the Compile Time."
slug: "strict-mode-policy--a-tool-to-catch-the-bug-in-the-compile-time"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Strict Mode is a special class introduced in Android 2.3 for debugging. This developer tools detect things done accidentally and bring them to our attention so that we can fix them. It is most commonly used to catch the accidental disk or network access on the applications’ main thread, where UI operations are received and animations takes place. 
StrictMode is basically a tool to catch the bug in the Compile Time mode.

StrictMode is basically a tool to catch the bug in the Compile Time mode. Using this we can avoid the memory leaks in our applications.

## The below Code Snippet is to setup the StrictMode for Thread Policies. This Code is to be set at the entry points to our application.
   

    StrictMode.setThreadPolicy(new StrictMode.ThreadPolicy.Builder()  
        .detectDiskWrites()  
        .penaltyLog() //Logs a message to LogCat  
        .build())

## The below code deals with leaks of memory, like it detects when in SQLLite finalize is called or not.
    StrictMode.setVmPolicy(new StrictMode.VmPolicy.Builder()  
        .detectActivityLeaks()  
        .detectLeakedClosableObjects()  
        .penaltyLog()  
        .build()); 

