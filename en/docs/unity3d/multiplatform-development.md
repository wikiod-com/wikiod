---
title: "Multiplatform development"
slug: "multiplatform-development"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Organizing platform specific methods to partial classes
[Partial classes][1] provide a clean way to separate core logic of your scripts from platform specific methods.

Partial classes and methods are marked with the keyword `partial`. This signals the compiler to leave the class "open" and look in other files for the rest of the implementation.

<!-- language: c# -->

    // ExampleClass.cs
    using UnityEngine;
    
    public partial class ExampleClass : MonoBehaviour
    {
        partial void PlatformSpecificMethod();
    
        void OnEnable()
        {
            PlatformSpecificMethod();
        }
    }

Now we can create files for our platform specific scripts that implement the partial method. Partial methods can have parameters (also `ref`) but must return `void`.

<!-- language: c# -->

    // ExampleClass.Iphone.cs

    #if UNITY_IPHONE
    using UnityEngine;
    
    public partial class ExampleClass
    {
        partial void PlatformSpecificMethod()
        {
            Debug.Log("I am an iPhone");
        }
    }
    #endif

<!-- language: c# -->

    // ExampleClass.Android.cs

    #if UNITY_ANDROID
    using UnityEngine;
    
    public partial class ExampleClass
    {
        partial void PlatformSpecificMethod()
        {
            Debug.Log("I am an Android");
        }
    }
    #endif

If a partial method is not implemented, the compiler will omit the call.




> Tip: This pattern is useful when creating Editor specific methods as well.

  [1]: https://www.wikiod.com/docs/c%23/3674/partial-class-and-methods#t=201608040607122344931

## Compiler Definitions
Compiler definitions run platform specific code. Using them you can make small differences between various platforms.

 - Trigger Game Center achievements on apple devices and google play achievements on Android devices. 
 - Change the icons in menus (windows logo in windows, Linux penguin in Linux). 
 - Possibly have platform specific mechanics depending on the platform.
 - And much more...


    void Update(){ 
    
    #if UNITY_IPHONE
        //code here is only called when running on iPhone
    #endif
    
    #if UNITY_STANDALONE_WIN && !UNITY_EDITOR
        //code here is only ran in a unity game running on windows outside of the editor
    #endif
    
    //other code that will be ran regardless of platform
    
    }

[A complete list of Unity compiler definitions can be found here][1]


  [1]: https://docs.unity3d.com/Manual/PlatformDependentCompilation.html

