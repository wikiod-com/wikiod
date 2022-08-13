---
title: "Android Studio"
slug: "android-studio"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Setup Android Studio
**System Requirements**
- Microsoft® Windows® 8/7/Vista/2003 (32 or 64-bit).
- Mac® OS X® 10.8.5 or higher, up to 10.9 (Mavericks)
- GNOME or KDE desktop  

**Installation**

Window
1. Download and install [JDK (Java Development Kit)][1] version 8
2. Download [Android Studio][2]
3. Launch `Android Studio.exe` then mention JDK path and download the latest SDK

Linux
 1. Download and install [JDK (Java Development Kit)][1] version 8
 2. Download [Android Studio][2]
 3. Extract the zip file
 4. Open terminal, cd to the extracted folder, cd to bin (example `cd android-studio/bin`)
 5. Run ./studio.sh 

  [1]: http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
  [2]: https://developer.android.com/studio/index.html

## View And Add Shortcuts in Android Studio
By going to Settings >> Keymap A window will popup showing All the `Editor Actions` with the their name and shortcuts. Some of the `Editor Actions` do not have shortcuts. So right click on that and add a new shortcut to that.  
Check the image below

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/bjDlR.png

## Filter logs from UI
Android logs can be filtered directly from the UI. Using this code

    public class MainActivity extends AppCompatActivity {
        private final static String TAG1 = MainActivity.class.getSimpleName();
        private final static String TAG2 = MainActivity.class.getCanonicalName();
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
            Log.e(TAG1,"Log from onCreate method with TAG1");
            Log.i(TAG2,"Log from onCreate method with TAG2");
        }
    }

If I use the regex `TAG1|TAG2` and the level `verbose` I get

    01-14 10:34:46.961 12880-12880/android.doc.so.thiebaudthomas.sodocandroid E/MainActivity: Log from onCreate method with TAG1
    01-14 10:34:46.961 12880-12880/android.doc.so.thiebaudthomas.sodocandroid I/androdi.doc.so.thiebaudthomas.sodocandroid.MainActivity: Log from onCreate method with TAG2

[![enter image description here][1]][1]

The level can be set to get logs with given level and above. For example the `verbose` level will catch `verbose, debug, info, warn, error and assert` logs.

Using the same example, if I set the level to `error`, I only get 

    01-14 10:34:46.961 12880-12880/androdi.doc.so.thiebaudthomas.sodocandroid E/MainActivity: Log from onCreate method with TAG1

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/NkuER.png
  [2]: http://i.stack.imgur.com/8i6o0.png

## Create filters configuration
Custom filters can be set and save from the UI. In the `AndroidMonitor` tab, click on the right dropdown (must contains `Show only selected application` or `No filters`) and select `Edit filter configuration`.

Enter the filter you want

[![enter image description here][1]][1]

And use it (you can selected it from the same dropdown)

[![enter image description here][2]][2]

**Important** If you add an input in the filter bar, android studio will consider both your filter and your input.

With both input and filter there is no output
[![Custom filter with input][3]][3]

Without filter, there is some outputs
[![Filter input only][4]][4]


  [1]: http://i.stack.imgur.com/0cm2p.png
  [2]: http://i.stack.imgur.com/5ujVM.png
  [3]: http://i.stack.imgur.com/oSuUP.png
  [4]: http://i.stack.imgur.com/31oP6.png

## Custom colors of logcat message based on message importance
Go to File -> Settings -> Editor -> Colors & Fonts -> Android Logcat

**Change the colors as you need:**

[![enter image description here][1]][1]

**Choose the appropriate color:**

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/Dg8rG.png
  [2]: http://i.stack.imgur.com/GZpbS.png

## Enable/Disable blank line copy
`ctrl + alt + shift + /` (`cmd + alt + shift + /` on `MacOS`) should show you the following dialog:


[![enter image description here][1]][1]


Clicking on `Registry` you will get

[![enter image description here][2]][2]

The key you want to enable/disable is 

    editor.skip.copy.and.cut.for.empty.selection

Tested on `Linux Ubuntu` and `MacOS`. 

  [1]: http://i.stack.imgur.com/YZ5Ax.png
  [2]: http://i.stack.imgur.com/LC1p9.png

## Android Studio useful shortcuts


## Android Studio Improve performance tip


## Gradle build project takes forever

**Android Studio** -> **Preferences** -> **Gradle** -> Tick  **Offline work** and then restart your Android studio.



**Reference screenshot:**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/VhZL5.png

## Create assets folder


