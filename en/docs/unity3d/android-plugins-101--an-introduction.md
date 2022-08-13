---
title: "Android Plugins 101 - An Introduction"
slug: "android-plugins-101---an-introduction"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This topic is the first part of a series on how to create Android Plugins for Unity. Start here if you have little to no experience creating plugins, and/or the Android OS.

Through this series, I extensively use external links that I encourage you to read. While paraphrased versions of the relevant content will be included here, there may be times when the additional reading will help.

----------
## Beginning with Android plugins ##
Currently, Unity provides two ways to call native Android code. 

 1. Write native Android code in Java, and call these Java functions using C#
 2. Write C# code to directly call functions that are part of the Android OS

To interact with native code, Unity provides some classes and functions. 


 - [AndroidJavaObject][1] - This is the base class that Unity provides to interact with native code. Almost any object returned from native code can be stored as and AndroidJavaObject
 - [AndroidJavaClass][2] - Inherits from AndroidJavaObject. This is used to reference classes in your native code
 - [Get][3] / [Set][4] values of an instance of a native object, and the static [GetStatic][5] / [SetStatic][6] versions
 - [Call][7] / [CallStatic][8] to call native non-static & static functions


----------
<br>

## Outline to creating a plugin and terminology ##

 1. Write native Java code in [Android Studio][9]
 2. Export the code in a JAR / AAR file (Steps here for [JAR files][10] and [AAR files][11])
 3. Copy the JAR / AAR file into your Unity project at **Assets/Plugins/Android**
 4. Write code in Unity (C# has always been the way to go here) to call functions in the plugin

Note that the first three steps apply ONLY if you wish to have a native plugin!

From here on out, I'll refer to the JAR / AAR file as the **native plugin**, and the C# script as the **C# wrapper**


----------
<br>

## Choosing between the plugin creation methods ##
It's immediately obvious that the first way of creating plugins is long drawn, so choosing your route seems moot. 
However, method 1 is the ONLY way to call custom code. So, how does one choose? 

Simply put, does your plugin
 1. Involve custom code - Choose method 1 
 2. Only invoke native Android functions? - Choose method 2

Please do **NOT** try to "mix" (i.e. a part of the plugin using method 1, and the other using method 2) the two methods! While entirely possible, it's often impractical and painful to manage.  


  [1]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.html
  [2]: https://docs.unity3d.com/ScriptReference/AndroidJavaClass.html
  [3]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.Get.html
  [4]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.Set.html
  [5]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.GetStatic.html
  [6]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.SetStatic.html
  [7]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.Call.html
  [8]: https://docs.unity3d.com/ScriptReference/AndroidJavaObject.CallStatic.html
  [9]: https://developer.android.com/studio/index.html
  [10]: https://stackoverflow.com/questions/21712714/how-to-make-a-jar-out-from-an-android-studio-project
  [11]: https://stackoverflow.com/questions/24309950/create-aar-file-in-android-studio

## UnityAndroidPlugin.cs
Create a new C# script in Unity and replace it's contents with the following
    
    using UnityEngine;
    using System.Collections;

    public static class UnityAndroidPlugin {

    }

## UnityAndroidNative.java
Create a new Java class in Android Studio and replace it's contents with the following
    
    package com.axs.unityandroidplugin;
    import android.util.Log;
    import android.widget.Toast;
    import android.app.ActivityManager;
    import android.content.Context;
    
    
    public class UnityAndroidNative {
    
    
    }

## UnityAndroidPluginGUI.cs
Create a new C# script in Unity and paste these contents

    using UnityEngine;
    using System.Collections;
    
    public class UnityAndroidPluginGUI : MonoBehaviour {

        void OnGUI () {
            
        }

    }

