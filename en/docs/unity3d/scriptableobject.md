---
title: "ScriptableObject"
slug: "scriptableobject"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

# ScriptableObjects with AssetBundles

Pay attention when adding prefabs to AssetBundles if they contain references to ScriptableObjects. Since ScriptableObjects are essentially assets, Unity creates duplicates of them before adding them to AssetBundles, which may result in undesired behaviour during runtime.

When you load such a GameObject from an AssetBundle, it may be necessary to reinject the ScriptableObject assets to the loaded scripts, replacing the bundled ones. See [Dependency Injection][1]

  [1]: https://www.wikiod.com/design-patterns/dependency-injection

## Introduction
ScriptableObjects are serialized objects that are not bound to scenes or gameobjects as MonoBehaviours are. To put it one way, they are data and methods bound to asset files inside your project. These ScriptableObject assets can be passed to MonoBehaviours or other ScriptableObjects, where their public methods can be accessed.

Due to their nature as serialized assets, they make for excellent manager classes and data sources.

# Creating ScriptableObject assets

Below is a simple ScriptableObject implementation.

<!-- language: c# -->
    using UnityEngine;

    [CreateAssetMenu(menuName = "StackOverflow/Examples/MyScriptableObject")]
    public class MyScriptableObject : ScriptableObject
    {
        [SerializeField]
        int mySerializedNumber;

        int helloWorldCount = 0;

        public void HelloWorld()
        {
            helloWorldCount++;
            Debug.LogFormat("Hello! My number is {0}.", mySerializedNumber);
            Debug.LogFormat("I have been called {0} times.", helloWorldCount);
        }
    }



By adding the `CreateAssetMenu` attribute to the class, Unity will list it in the **Assets/Create** submenu. In this case it's under **Assets/Create/StackOverflow/Examples**.

Once created, ScriptableObject instances can be passed to other scripts and ScriptableObjects through the Inspector.

<!-- language: c# -->

    using UnityEngine;
    
    public class SampleScript : MonoBehaviour {
    
        [SerializeField]
        MyScriptableObject myScriptableObject;
    
        void OnEnable()
        {
            myScriptableObject.HelloWorld();
        }
    }


  [1]: https://docs.unity3d.com/ScriptReference/CreateAssetMenuAttribute.html

## Create ScriptableObject instances through code
You create new ScriptableObject instances through `ScriptableObject.CreateInstance<T>()`

<!-- language: c# -->
    T obj = ScriptableObject.CreateInstance<T>();

Where `T` extends `ScriptableObject`.

> Do not create ScriptableObjects by calling their constructors, ie. `new ScriptableObject()`.

Creating ScriptableObjects by code during runtime is rarely called for because their main use is data serialization. You might as well use standard classes at this point. It is more common when you are scripting editor extensions.

## ScriptableObjects are serialized in editor even in PlayMode
Extra care should be taken when accessing serialized fields in a ScriptableObject instance.

If a field is marked `public` or serialized through `SerializeField`, changing its value is permanent. They do not reset when exiting playmode like MonoBehaviours do. This can be useful at times, but it can also make a mess.

Because of this it's best to make serialized fields read-only and avoid public fields altogether.

<!-- language: c# -->
    public class MyScriptableObject : ScriptableObject
    {
        [SerializeField]
        int mySerializedValue;
    
        public int MySerializedValue
        {
            get { return mySerializedValue; }
        }
    }

If you wish to store public values in a ScriptableObject that are reset between play sessions, consider using the following pattern.

<!-- language: c# -->
    public class MyScriptableObject : ScriptableObject
    {
        // Private fields are not serialized and will reset to default on reset
        private int mySerializedValue;
    
        public int MySerializedValue
        {
            get { return mySerializedValue; }
            set { mySerializedValue = value; }
        }
    }

## Find existing ScriptableObjects during runtime
To find *active* ScriptableObjects during runtime, you can use `Resources.FindObjectsOfTypeAll()`.

<!-- language: c# -->
    T[] instances = Resources.FindObjectsOfTypeAll<T>();

Where `T` is the type of the ScriptableObject instance you're searching. *Active* means it has been loaded in memory in some form before.

This method is very slow so remember to cache the return value and avoid calling it frequently. Referencing the ScriptableObjects directly in your scripts should be your preferred option.

> *Tip:* You can maintain your own instance collections for faster lookups. Have your ScriptableObjects register themselves to a shared collection during `OnEnable()`.


