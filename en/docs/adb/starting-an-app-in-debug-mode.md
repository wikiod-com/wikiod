---
title: "Starting an app in debug mode"
slug: "starting-an-app-in-debug-mode"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

## How to wait for debugger before starting the app?
Let's say your launch activity is called `MainActivity`, in your app com.example.myapp. 
In the manifest:

    <activity
            android:name=".MainActivity"
            >
            <intent-filter>
              <action android:name="android.intent.action.MAIN"/>
              <category android:name="android.intent.category.LAUNCHER"/>
            </intent-filter>
    </activity>

Now let's say you want to launch the app, so that it waits for the debugger to connect before the app really starts.

You can use `adb shell` to achieve that.  
In our case, simply run:

    adb shell am start -D -n com.example.myapp/com.example.myapp.MainActivity

Now, all that's left is to attach your favorite debugger. For example, if you use Intellij or Android Studio go to Run->Attach debugger to Android process-> select your app package name

