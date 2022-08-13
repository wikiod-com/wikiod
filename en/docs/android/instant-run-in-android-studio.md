---
title: "Instant Run in Android Studio"
slug: "instant-run-in-android-studio"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Instant Run is an extended behavior for the run and debug commands that enables faster debugging by not requiring a full build and reinstall for eevry change done in your app's code.

> Introduced in Android Studio 2.0, Instant Run is a behavior for the
> Run  and Debug commands that significantly reduces the time between
> updates to your app. Although your first build may take longer to
> complete, Instant Run pushes subsequent updates to your app without
> building a new APK, so changes are visible much more quickly.
> 
> Instant Run is supported only when you deploy the debug build variant,
> use Android Plugin for Gradle version 2.0.0 or higher, and set
> minSdkVersion to 15 or higher in your app's module-level build.gradle
> file. For the best performance, set minSdkVersion to 21 or higher.
> 
> After deploying an app, a small, yellow thunderbolt icon appears
> within the Run  button (or Debug   button), indicating that Instant
> Run is ready to push updates the next time you click the button.
> Instead of building a new APK, it pushes just those new changes and,
> in some cases, the app doesn't even need to restart but immediately
> shows the effect of those code changes.
> 
> Instant Run pushes updated code and resources to your connected device
> or emulator by performing a hot swap, warm swap, or cold swap. It
> automatically determines the type of swap to perform based on the type
> of change you made. The video above provides interesting detail about
> how this all works under the hood. For a quick summary of how Instant
> Run behaves when you push certain code changes to a target device,
> however, see the following table.

[Documentation][1]


  [1]: https://developer.android.com/studio/run/index.html#instant-run

## Enabling or disabling Instant Run
1. Open the Settings or Preferences dialog:
   - On Windows or Linux, select `File` > `Settings` from the main menu.
   - On Mac OSX, select `Android Studio` > `Preferences` from the main menu.
2. Navigate to `Build, Execution, Deployment` > `Compiler`.
3. In the text field next to Command-line Options, enter your command-line options.
4. Click OK to save and exit.


[![enter image description here][1]][1]

The top option is Instant run. Check/uncheck that box.

[Documentation][2]


  [1]: http://i.stack.imgur.com/9lTtp.png
  [2]: https://developer.android.com/studio/run/index.html#instant-run

## Types of code Swaps in Instant Run
There are three types of code swaps that Instant run enables to support faster debugging and running app from your code in Android Studio.

 - Hot Swap
 - Warm Swap
 - Cold Swap

**When are each of these swaps triggered?**

**HOT SWAP** is triggered when an existing method's implementation is changed.

**WARM SWAP** is triggered when an existing resource is changed or removed (anything in the res folder)

**COLD SWAP** whenever there is a structural code change in your app's code e.g. 

1. Add, remove, or change:
 - an annotation
 - an instance field
 - a static field
 - a static method signature
 - an instance method signature
2. Change which parent class the current class inherits from
3. Change the list of implemented interfaces
4. Change a class's static initializer
5. Reorder layout elements that use dynamic resource IDs

**What happens when a code swap happens?**

**HOT SWAP** changes are visible instantly - as soon as the next call to the method whose implementation is changed is made.

**WARM SWAP** restarts the current activity

**COLD SWAP** restarts the entire app (without reinstall)



## Unsupported code changes when using Instant Run
There are a few changes where instant won't do its trick and a full build and reinstall fo your app will happen just like it used to happen before Instant Run was born.

1. Change the app manifest
2. Change resources referenced by the app manifest
3. Change an Android widget UI element (requires a Clean and Rerun)

[Documentation][1]


  [1]: https://developer.android.com/studio/run/index.html#instant-run

