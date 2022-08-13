---
title: "Singletons in Unity"
slug: "singletons-in-unity"
draft: false
images: []
weight: 9768
type: docs
toc: true
---

While there are schools of thought which make compelling arguments why unconstrained use of Singletons is a bad idea, e.g. [Singleton on gameprogrammingpatterns.com][1], there are occasions when you might want to persist a GameObject in Unity over multiple Scenes (e.g. for seamless background music) while ensuring that no more than one instance can exist; a perfect use case for a Singleton.

By adding this script to a GameObject, once it has been instantiated (e.g. by including it anywhere in a Scene) it will remain active across Scenes, and only one instance will ever exist.

----------
[ScriptableObject][2] ([UnityDoc][3]) instances provide a valid alternative to Singletons for some use cases. While they don't implicitly enforce the single instance rule, they retain their state between scenes and play nicely with the Unity serialization process. They also promote [Inversion of Control][4] as dependencies are [injected through the editor][5].

<!-- language: c# -->
    // MyAudioManager.cs
    using UnityEngine;

    [CreateAssetMenu] // Remember to create the instance in editor
    public class MyAudioManager : ScriptableObject {
        public void PlaySound() {}
    }


<!-- language: c# -->
    // MyGameObject.cs
    using UnityEngine;

    public class MyGameObject : MonoBehaviour
    {
        [SerializeField]
        MyAudioManager audioManager; //Insert through Inspector

        void OnEnable()
        {
            audioManager.PlaySound();
        }
    }


---

Further reading
====

- [Singleton Implementation in C#][6]


  [1]: http://gameprogrammingpatterns.com/singleton.html
  [2]: https://www.wikiod.com/unity3d/scriptableobject
  [3]: https://docs.unity3d.com/ScriptReference/ScriptableObject.html
  [4]: https://en.wikipedia.org/wiki/Inversion_of_control
  [5]: https://en.wikipedia.org/wiki/Dependency_injection
  [6]: https://www.wikiod.com/docs/c%23/1192/singleton-implementation#t=201607242046137195369

## Implementation using RuntimeInitializeOnLoadMethodAttribute
Since **Unity 5.2.5** it's possible to use [RuntimeInitializeOnLoadMethodAttribute][1] to execute initialization logic bypassing [MonoBehaviour order of execution][2]. It provides a way to create more clean and robust implementation:
<!-- language: lang-cs -->

    using UnityEngine;
    
    sealed class GameDirector : MonoBehaviour
    {
        // Because of using RuntimeInitializeOnLoadMethod attribute to find/create and
        // initialize the instance, this property is accessible and
        // usable even in Awake() methods.
        public static GameDirector Instance
        {
            get; private set;
        }
    
        // Thanks to the attribute, this method is executed before any other MonoBehaviour
        // logic in the game.
        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        static void OnRuntimeMethodLoad()
        {
            var instance = FindObjectOfType<GameDirector>();
    
            if (instance == null)
                instance = new GameObject("Game Director").AddComponent<GameDirector>();
    
            DontDestroyOnLoad(instance);
    
            Instance = instance;
        }
    
        // This Awake() will be called immediately after AddComponent() execution
        // in the OnRuntimeMethodLoad(). In other words, before any other MonoBehaviour's
        // in the scene will begin to initialize.
        private void Awake()
        {
            // Initialize non-MonoBehaviour logic, etc.
            Debug.Log("GameDirector.Awake()", this);
        }
    }

The resulting order of execution:
 1. `GameDirector.OnRuntimeMethodLoad()` started...
 2. `GameDirector.Awake()`
 3. `GameDirector.OnRuntimeMethodLoad()` completed.
 4. `OtherMonoBehaviour1.Awake()`
 5. `OtherMonoBehaviour2.Awake()`, etc.

  [1]: http://docs.unity3d.com/ScriptReference/RuntimeInitializeOnLoadMethodAttribute.html
  [2]: https://docs.unity3d.com/Manual/ExecutionOrder.html

## A simple Singleton MonoBehaviour in Unity C#
In this example, a private static instance of the class is declared at its beginning.

The value of a static field is shared between instances, so if a new instance of this class gets created the `if` will find a reference to the first Singleton object, destroying the new instance (or its game object).

<!-- language: c# -->  

    using UnityEngine;
            
    public class SingletonExample : MonoBehaviour {
    
        private static SingletonExample _instance;
        
        void Awake(){
    
            if (_instance == null){
    
                _instance = this;
                DontDestroyOnLoad(this.gameObject);
        
                //Rest of your Awake code
        
            } else {
                Destroy(this);
            }
        }
    
        //Rest of your class code
    
    }

## Singleton Pattern utilizing Unitys Entity-Component system
The core idea is to use GameObjects to represent singletons, which has multiple advantages:

- Keeps complexity to a minimum but supports concepts like dependency injection
- Singletons have a normal Unity lifecycle as part of the Entity-Component system
- Singletons can be lazy loaded and cached locally where regulary needed (e.g. in update loops)
- No static fields needed
- No need to modify existing MonoBehaviours / Components to use them as Singletons
- Easy to reset (just destroy the Singletons GameObject), will be lazy loaded again on next usage
- Easy to inject mocks (just initialize it with the mock before using it)
- Inspection and configuration using normal Unity editor and can happen already on editor time ( [Screenshot of a Singleton accessible in the Unity editor][1] )


Test.cs (which uses the example singleton):

    using UnityEngine;
    using UnityEngine.Assertions;
    
    public class Test : MonoBehaviour {
        void Start() {
            ExampleSingleton singleton = ExampleSingleton.instance;
            Assert.IsNotNull(singleton); // automatic initialization on first usage
            Assert.AreEqual("abc", singleton.myVar1);
            singleton.myVar1 = "123";
            // multiple calls to instance() return the same object:
            Assert.AreEqual(singleton, ExampleSingleton.instance); 
            Assert.AreEqual("123", ExampleSingleton.instance.myVar1);
        }
    }

ExampleSingleton.cs (which contains an example and the actual Singleton class):

    using UnityEngine;
    using UnityEngine.Assertions;
    
    public class ExampleSingleton : MonoBehaviour {
        public static ExampleSingleton instance { get { return Singleton.get<ExampleSingleton>(); } }
        public string myVar1 = "abc";
        public void Start() { Assert.AreEqual(this, instance, "Singleton more than once in scene"); } 
    }
    
    /// <summary> Helper that turns any MonBehaviour or other Component into a Singleton </summary>
    public static class Singleton {
        public static T get<T>() where T : Component {
            return GetOrAddGo("Singletons").GetOrAddChild("" + typeof(T)).GetOrAddComponent<T>();
        }
        private static GameObject GetOrAddGo(string goName) {
            var go = GameObject.Find(goName);
            if (go == null) { return new GameObject(goName); }
            return go;
        }
    }
    
    public static class GameObjectExtensionMethods { 
        public static GameObject GetOrAddChild(this GameObject parentGo, string childName) {
            var childGo = parentGo.transform.FindChild(childName);
            if (childGo != null) { return childGo.gameObject; } // child found, return it
            var newChild = new GameObject(childName);        // no child found, create it
            newChild.transform.SetParent(parentGo.transform, false); // add it to parent
            return newChild;
        }
    
        public static T GetOrAddComponent<T>(this GameObject parentGo) where T : Component {
            var comp = parentGo.GetComponent<T>();
            if (comp == null) { return parentGo.AddComponent<T>(); }
            return comp;
        }
    }

The two extension methods for GameObject are helpful in other situations as well, if you don't need them move them inside the Singleton class and make them private.


  [1]: https://i.imgur.com/ShhafVx.png

## Singleton Implementation through base class
In projects that feature several singleton classes (as is often the case), it can be clean and convenient to abstract the singleton behaviour to a base class:

<!-- language: lang-cs -->

    using UnityEngine;
    using System.Collections.Generic;
    using System;
    
    public abstract class MonoBehaviourSingleton<T> : MonoBehaviour {
        
        private static Dictionary<Type, object> _singletons
            = new Dictionary<Type, object>();
    
        public static T Instance {
            get {
                return (T)_singletons[typeof(T)];
            }
        }
    
        void OnEnable() {
            if (_singletons.ContainsKey(GetType())) {
                Destroy(this);
            } else {
                _singletons.Add(GetType(), this);
                DontDestroyOnLoad(this);
            }
        }
    }

A MonoBehaviour may then implement the singleton pattern by extending MonoBehaviourSingleton<T>. This approach allows the pattern to be utilised with a minimal footprint on the Singleton itself:

<!-- language: lang-cs -->

    using UnityEngine;
    using System.Collections;
    
    public class SingletonImplementation : MonoBehaviourSingleton<SingletonImplementation> {
    
        public string Text= "String Instance";
    
        // Use this for initialisation
        IEnumerator Start () {
            var demonstration = "SingletonImplementation.Start()\n" +
                                "Note that the this text logs only once and\n"
                                "only one class instance is allowed to exist.";
            Debug.Log(demonstration);
            yield return new WaitForSeconds(2f);
            var secondInstance = new GameObject();
            secondInstance.AddComponent<SingletonImplementation>();
        }
       
    }

Note that one of the benefits of the singleton pattern is that a reference to the instance may be accessed statically:

<!-- language: lang-cs -->
    // Logs: String Instance
    Debug.Log(SingletonImplementation.Instance.Text);

Keep in mind though, this practise should be minimised in order to reduce coupling. This approach also comes at a slight performance cost due to the use of Dictionary, but as this collection may contain only one instance of each singleton class, the trade-off in terms of the DRY principle (Don't Repeat Yourself), readability and convenience is small.

## MonoBehaviour & ScriptableObject based Singleton Class
Most Singleton examples use MonoBehaviour as the base class. The main disadvantage is that this Singleton class only lives during run time. This has some drawbacks:

 - There is no way of directly editing the singleton fields other than
   changing the code. 
 - No way to store a reference to other assets on the
   Singleton. 
 - No way of setting the singleton as the destination of a
   Unity UI event. I end up using what i call "Proxy Components" that
   its sole propose is to have 1 line methods that call "GameManager.Instance.SomeGlobalMethod()".

As noted on the remarks there are implementations that try to solve this using ScriptableObjects as base class but lose the run time benefits of the MonoBehaviour. This implementation solves this problems by using a ScriptableObject as a base class and an associated MonoBehavior during run time:

 - It is an asset so its properties can be updated on the editor like any other Unity asset.
 - It plays nicely with the Unity serialization process.
 - Is possible to assign references on the singleton to other assets from the editor (dependencies are injected through the editor).
 - Unity events can directly call methods on the Singleton.
 - Can call it from anywhere in the codebase using "SingletonClassName.Instance"
 - Has access to run time MonoBehaviour events and methods like: Update, Awake, Start, FixedUpdate, StartCoroutine, etc.
```
/************************************************************
 * Better Singleton by David Darias
 * Use as you like - credit where due would be appreciated :D
 * Licence: WTFPL V2, Dec 2014
 * Tested on Unity v5.6.0 (should work on earlier versions)
 * 03/02/2017 - v1.1 
 * **********************************************************/

using System;
using UnityEngine;
using SingletonScriptableObjectNamespace;

public class SingletonScriptableObject<T> : SingletonScriptableObjectNamespace.BehaviourScriptableObject where T : SingletonScriptableObjectNamespace.BehaviourScriptableObject
{
    //Private reference to the scriptable object
    private static T _instance;
    private static bool _instantiated;
    public static T Instance
    {
        get
        {
            if (_instantiated) return _instance;
            var singletonName = typeof(T).Name;
            //Look for the singleton on the resources folder
            var assets = Resources.LoadAll<T>("");
            if (assets.Length > 1) Debug.LogError("Found multiple " + singletonName + "s on the resources folder. It is a Singleton ScriptableObject, there should only be one.");
            if (assets.Length == 0)
            {
                _instance = CreateInstance<T>();
                Debug.LogError("Could not find a " + singletonName + " on the resources folder. It was created at runtime, therefore it will not be visible on the assets folder and it will not persist.");
            }
            else _instance = assets[0];
            _instantiated = true;
            //Create a new game object to use as proxy for all the MonoBehaviour methods
            var baseObject = new GameObject(singletonName);
            //Deactivate it before adding the proxy component. This avoids the execution of the Awake method when the the proxy component is added.
            baseObject.SetActive(false);
            //Add the proxy, set the instance as the parent and move to DontDestroyOnLoad scene
            SingletonScriptableObjectNamespace.BehaviourProxy proxy = baseObject.AddComponent<SingletonScriptableObjectNamespace.BehaviourProxy>();
            proxy.Parent = _instance;
            Behaviour = proxy;
            DontDestroyOnLoad(Behaviour.gameObject);
            //Activate the proxy. This will trigger the MonoBehaviourAwake. 
            proxy.gameObject.SetActive(true);
            return _instance;
        }
    }
    //Use this reference to call MonoBehaviour specific methods (for example StartCoroutine)
    protected static MonoBehaviour Behaviour;
    public static void BuildSingletonInstance() { SingletonScriptableObjectNamespace.BehaviourScriptableObject i = Instance; }
    private void OnDestroy(){ _instantiated = false; }
}

// Helper classes for the SingletonScriptableObject
namespace SingletonScriptableObjectNamespace
{
    #if UNITY_EDITOR
    //Empty custom editor to have cleaner UI on the editor.
    using UnityEditor;
    [CustomEditor(typeof(BehaviourProxy))]
    public class BehaviourProxyEditor : Editor
    {
        public override void OnInspectorGUI(){}
    }
    
    #endif
    
    public class BehaviourProxy : MonoBehaviour
    {
        public IBehaviour Parent;

        public void Awake() { if (Parent != null) Parent.MonoBehaviourAwake(); }
        public void Start() { if (Parent != null) Parent.Start(); }
        public void Update() { if (Parent != null) Parent.Update(); }
        public void FixedUpdate() { if (Parent != null) Parent.FixedUpdate(); }
    }

    public interface IBehaviour
    {
        void MonoBehaviourAwake();
        void Start();
        void Update();
        void FixedUpdate();
    }

    public class BehaviourScriptableObject : ScriptableObject, IBehaviour
    {
        public void Awake() { ScriptableObjectAwake(); }
        public virtual void ScriptableObjectAwake() { }
        public virtual void MonoBehaviourAwake() { }
        public virtual void Start() { }
        public virtual void Update() { }
        public virtual void FixedUpdate() { }
    }
}
```

Here there is an example GameManager singleton class using the SingletonScriptableObject (with a lot of comments):

```
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

//this attribute is optional but recommended. It will allow the creation of the singleton via the asset menu.
//the singleton asset should be on the Resources folder.
[CreateAssetMenu(fileName = "GameManager", menuName = "Game Manager", order = 0)]
public class GameManager : SingletonScriptableObject<GameManager> {

    //any properties as usual
    public int Lives;
    public int Points;

    //optional (but recommended)
    //this method will run before the first scene is loaded. Initializing the singleton here
    //will allow it to be ready before any other GameObjects on every scene and will
    //will prevent the "initialization on first usage". 
    [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
    public static void BeforeSceneLoad() { BuildSingletonInstance(); }

    //optional,
    //will run when the Singleton Scriptable Object is first created on the assets. 
    //Usually this happens on edit mode, not runtime. (the override keyword is mandatory for this to work)
    public override void ScriptableObjectAwake(){
        Debug.Log(GetType().Name + " created." );
    }

    //optional,
    //will run when the associated MonoBehavioir awakes. (the override keyword is mandatory for this to work)
    public override void MonoBehaviourAwake(){
        Debug.Log(GetType().Name + " behaviour awake." );

        //A coroutine example:
        //Singleton Objects do not have coroutines.
        //if you need to use coroutines use the atached MonoBehaviour
        Behaviour.StartCoroutine(SimpleCoroutine());
    }

    //any methods as usual
    private IEnumerator SimpleCoroutine(){
        while(true){
            Debug.Log(GetType().Name + " coroutine step." );
            yield return new WaitForSeconds(3);
        }
    }

    //optional,
    //Classic runtime Update method (the override keyword is mandatory for this to work).
    public override void Update(){

    }

    //optional,
    //Classic runtime FixedUpdate method (the override keyword is mandatory for this to work).
    public override void FixedUpdate(){

    }
}

/*
*  Notes:
*  - Remember that you have to create the singleton asset on edit mode before using it. You have to put it on the Resources folder and of course it should be only one. 
*  - Like other Unity Singleton this one is accessible anywhere in your code using the "Instance" property i.e: GameManager.Instance
*/
```

## Advanced Unity Singleton
This example combines multiple variants of MonoBehaviour singletons found on the internet into one and let you change its behavior depending on global static fields.

This example was tested using Unity 5.
To use this singleton, all you need to do is extend it as follows:
`public class MySingleton : Singleton<MySingleton> {}`. You may also need to override `AwakeSingleton` to use it instead of usual `Awake`. For further tweaking, change default values of static fields as described below.

----------


 1. This implementation makes use of
    [DisallowMultipleComponent](https://docs.unity3d.com/ScriptReference/DisallowMultipleComponent.html)
    attribute to keep one instance per GameObject. 
2. This class is
    abstract and can only be extended. It also contains one virtual
    method `AwakeSingleton` that needs to be overridden instead of
    implementing normal `Awake`. 
2. This implementation is thread safe.
3. This singleton is optimized. By using `instantiated` flag instead
    of instance null check we avoid the overhead that comes with Unity's
    implementation of `==` operator. ([Read
    more](http://blogs.unity3d.com/2014/05/16/custom-operator-should-we-keep-it/))
4. This implementation does not allow any calls to the singleton
    instance when it's about to get destroyed by Unity. 
5. This singleton
    comes with the following options:
 - `FindInactive`: whether to look for other instances of components of same type attached to inactive GameObject.
 - `Persist`: whether to keep component alive between scenes.
 - `DestroyOthers`: whether to destroy any other components of same type and keep only one.
 - `Lazy`: whether to set singleton instance "on the fly" (in `Awake`) or only "on demand" (when getter is called).

<!-- language: lang-cs -->    
    using UnityEngine;
    
    [DisallowMultipleComponent]
    public abstract class Singleton<T> : MonoBehaviour where T : Singleton<T>
    {
        private static volatile T instance;
        // thread safety
        private static object _lock = new object();
        public static bool FindInactive = true;
        // Whether or not this object should persist when loading new scenes. Should be set in Init().
        public static bool Persist;
        // Whether or not destory other singleton instances if any. Should be set in Init().
        public static bool DestroyOthers = true;
        // instead of heavy comparision (instance != null)
        // http://blogs.unity3d.com/2014/05/16/custom-operator-should-we-keep-it/
        private static bool instantiated;
    
        private static bool applicationIsQuitting;
    
        public static bool Lazy;
    
        public static T Instance
        {
            get
            {
                if (applicationIsQuitting)
                {
                    Debug.LogWarningFormat("[Singleton] Instance '{0}' already destroyed on application quit. Won't create again - returning null.", typeof(T));
                    return null;
                }
                lock (_lock)
                {
                    if (!instantiated)
                    {
                        Object[] objects;
                        if (FindInactive) { objects = Resources.FindObjectsOfTypeAll(typeof(T)); }
                        else { objects = FindObjectsOfType(typeof(T)); }
                        if (objects == null || objects.Length < 1)
                        {
                            GameObject singleton = new GameObject();
                            singleton.name = string.Format("{0} [Singleton]", typeof(T));
                            Instance = singleton.AddComponent<T>();
                            Debug.LogWarningFormat("[Singleton] An Instance of '{0}' is needed in the scene, so '{1}' was created{2}", typeof(T), singleton.name, Persist ? " with DontDestoryOnLoad." : ".");
                        }
                        else if (objects.Length >= 1)
                        {
                            Instance = objects[0] as T;
                            if (objects.Length > 1)
                            {
                                Debug.LogWarningFormat("[Singleton] {0} instances of '{1}'!", objects.Length, typeof(T));
                                if (DestroyOthers)
                                {
                                    for (int i = 1; i < objects.Length; i++)
                                    {
                                        Debug.LogWarningFormat("[Singleton] Deleting extra '{0}' instance attached to '{1}'", typeof(T), objects[i].name);
                                        Destroy(objects[i]);
                                    }
                                }
                            }
                            return instance;
                        }
                    }
                    return instance;
                }
            }
            protected set
            {
                instance = value;
                instantiated = true;
                instance.AwakeSingleton();
                if (Persist) { DontDestroyOnLoad(instance.gameObject); }
            }
        }
    
        // if Lazy = false and gameObject is active this will set instance
        // unless instance was called by another Awake method
        private void Awake()
        {
            if (Lazy) { return; }
            lock (_lock)
            {
                if (!instantiated)
                {
                    Instance = this as T;
                }
                else if (DestroyOthers && Instance.GetInstanceID() != GetInstanceID())
                {
                    Debug.LogWarningFormat("[Singleton] Deleting extra '{0}' instance attached to '{1}'", typeof(T), name);
                    Destroy(this);
                }
            }
        }
        
        // this might be called for inactive singletons before Awake if FindInactive = true
        protected virtual void AwakeSingleton() {}
    
        protected virtual void OnDestroy()
        {
            applicationIsQuitting = true;
            instantiated = false;
        }
    }

