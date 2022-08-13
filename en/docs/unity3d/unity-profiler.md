---
title: "Unity Profiler"
slug: "unity-profiler"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Using Profiler on different Device
----------------------------------

There are few importants things to know to properly hook the Profiler on different platforms.

Android
-------
In order to properly attach the profile, "Build and Run" button from the Build Settings window with the option **Autoconnect Profiler** checked must be used.

[![enter image description here][1]][1]

Another mandatory option, in [Android Player Settings][2] inspector in the Other Settings tab, there is a checkbox **Enable Internal profiler** which needs to be checked so LogCat will output profiler info.

[![enter image description here][3]][3]

Using only "Build" will not allow the profiler to connect to an Android device because the "Build and Run" use specific command line arguments to start it with LogCat.

iOS
---
In order to properly attach the profile, "Build and Run" button from the Build Settings window with the option **Autoconnect Profiler** checked must be used on the first run.

[![enter image description here][4]][4]

On iOS, there is no option in player settings that must be set for the Profiler to be enable. It should work out of the box.


  [1]: http://i.stack.imgur.com/13A2Z.png
  [2]: https://docs.unity3d.com/Manual/class-PlayerSettingsAndroid.html
  [3]: http://i.stack.imgur.com/S7JB8.png
  [4]: http://i.stack.imgur.com/bJFLV.png

## Profiler Markup
Using the [Profiler][1] Class
------------------------

One very good practice is to use Profiler.BeginSample and Profiler.EndSample because it will have its own entry in the Profiler Window.

Also, those tag will be stripped out on non-Development build using using ConditionalAttribute, so you don't need to remove them from your code.

<!-- language: c# -->

    public class SomeClass : MonoBehaviour 
    {
        void SomeFunction() 
        {
            Profiler.BeginSample("SomeClass.SomeFunction");
            // Various call made here
            Profiler.EndSample();
        }
    }

This will create an Entry "SomeClass.SomeFunction" in the Profiler Window that will allow easier debugging and identification of Bottle neck.


  [1]: https://docs.unity3d.com/ScriptReference/Profiler.html

