---
title: "Optimization"
slug: "optimization"
draft: false
images: []
weight: 9867
type: docs
toc: true
---

1. If possible, disable scripts on objects when they are not needed. For example if you have a script on an enemy object that searchers for and fires at the player consider disabling this script when the enemy is too far for example from the player.


## Coroutine Power
Usage
=====
If you have a long running operation that relies on the not-thread-safe Unity API, use [Coroutines][1] to split it over multiple frames and keep your application responsive.

[Coroutines][1] also help performing expensive actions every nth frame instead of running that action each frame.

Splitting Long-running Routines Over Multiple Frames
====================================================
Coroutines help distribute long running operations over multiple frames to help keep up the framerate of your application.

Routines that paint or generate terrain procedurally or generate noise are examples that may need the Coroutine treatment.

<!-- language: c# -->

    for (int y = 0; y < heightmap.Height; y++) 
    {
        for (int x = 0; x < heightmap.Width; x++)
        {
            // Generate pixel at (x, y)
            // Assign pixel at (x, y)
            
            // Process only 32768 pixels each frame
            if ((y * heightmap.Height + x) % 32 * 1024) == 0)
                yield return null; // Wait for next frame
        }
    }

> The code above is an easy to understand example. In production code it is better to avoid the per-pixel check that checks when to `yield return` (maybe do it every 2-3 rows) and to pre-calculate `for` loop length in advance.

Performing Expensive Actions Less Frequently
============================================
Coroutines help you perform expensive actions less frequently, so that it isn't as big a performance hit as it would be if performed every frame.

Taking the following example directly from the [Manual][1]:

<!-- language: c# -->

    private void ProximityCheck() 
    {
        for (int i = 0; i < enemies.Length; i++) 
        {
            if (Vector3.Distance(transform.position, enemies[i].transform.position) < dangerDistance)
                    return true;
        }
        return false;
    }
    
    private IEnumerator ProximityCheckCoroutine() 
    {
        while(true) 
        {
            ProximityCheck();
            yield return new WaitForSeconds(.1f);
        }
    }

> Proximity tests can be optimized even further by using the [CullingGroup API][2].

Common Pitfalls
===============
A common mistake developers make is accessing results or side effects of coroutines *outside* the coroutine. Coroutines return control to the caller as soon as a `yield return` statement is encountered and the result or side effect may not be performed yet. To circumvent problems where you *have* to use the result/side effect outside the coroutine, check [this answer][3].


  [1]: https://docs.unity3d.com/Manual/Coroutines.html
  [2]: https://docs.unity3d.com/Manual/CullingGroupAPI.html
  [3]: http://stackoverflow.com/a/38077315/4038191

## Strings
<!-- language-all: c# -->

One might argue that there are greater resource hogs in Unity than the humble string, but it is one of the easier aspects to fix early on.

# String operations build garbage

Most string operations build tiny amounts of garbage, but if those operations are called several times over the course of a single update, it stacks up. Over time it will trigger the automatic Garbage Collection, which may result in a visible CPU spike.

## Cache your string operations

Consider the following example.

    string[] StringKeys = new string[] {
        "Key0",
        "Key1",
        "Key2"
    };

    void Update()
    {
        for (var i = 0; i < 3; i++)
        {
            // Cached, no garbage generated
            Debug.Log(StringKeys[i]);
        }

        for (var i = 0; i < 3; i++)
        {
            // Not cached, garbage every cycle
            Debug.Log("Key" + i);
        }

        // The most memory-efficient way is to not create a cache at all and use literals or constants.
        // However, it is not necessarily the most readable or beautiful way.
        Debug.Log("Key0");
        Debug.Log("Key1");
        Debug.Log("Key2");
    }

It may look silly and redundant, but if you're working with Shaders, you might run into situations such as these. Caching the keys will make a difference.

Please note that string *literals* and *constants* do not generate any garbage, as they are injected statically into the program stack space. If you are *generating* strings at run-time and are *guaranteed* to be generating the **same** strings each time like the above example, caching will definitely help.

For other cases where the string generated is not the same each time, there is no other alternative to generating those strings. As such, the memory spike with manually generating strings each time is usually negligible, unless tens of thousands of strings are being generated at a time.

## Most string operations are Debug messages

Doing string operations for Debug messages, ie. `Debug.Log("Object Name: " + obj.name)` is fine and cannot be avoided during development. It is, however, important to ensure that irrelevant debug messages do not end up in the released product.

One way is to use the [Conditional attribute][1] in your debug calls. This not only removes the method calls, but also all the string operations going into it.

    using UnityEngine;
    using System.Collections;
    
    public class ConditionalDebugExample: MonoBehaviour
    {
        IEnumerator Start()
        {
            while(true)
            {
                // This message will pop up in Editor but not in builds
                Log("Elapsed: " + Time.timeSinceLevelLoad);
                yield return new WaitForSeconds(1f);
            }
        }
    
        [System.Diagnostics.Conditional("UNITY_EDITOR")]
        void Log(string Message)
        {
            Debug.Log(Message);
        }
    }

> This is a simplified example. You might want to invest some time designing a more fully fledged logging routine.

# String comparison

This is a minor optimisation, but it's worth a mention. Comparing strings is slightly more involved than one might think. The system will try to take cultural differences into account by default. You can opt to use a simple binary comparison instead, which performs faster.
    
    // Faster string comparison
    if (strA.Equals(strB, System.StringComparison.Ordinal)) {...}
    // Compared to
    if (strA == strB) {...}

    // Less overhead
    if (!string.IsNullOrEmpty(strA)) {...}
    // Compared to
    if (strA == "") {...}

    // Faster lookups
    Dictionary<string, int> myDic = new Dictionary<string, int>(System.StringComparer.Ordinal);
    // Compared to
    Dictionary<string, int> myDictionary = new Dictionary<string, int>();

  [1]: https://www.wikiod.com/docs/c%23/755/pragma-directives/8669/using-the-conditional-attribute#t=201607250855107189175

## Fast and Efficient Checks
Avoid unnecessary operations and method calls wherever you can, especially in a method which is called many times a second, like `Update`. 

Distance/Range Checks
===============
Use `sqrMagnitude` instead of `magnitude` when comparing distances. This avoids unnecessary `sqrt` operations. Note that when using `sqrMagnitude`, the right hand side must also be squared.

    if ((target.position - transform.position).sqrMagnitude < minDistance * minDistance))

Bounds Checks
=============
Object intersections can be crudely checked by checking whether their `Collider`/`Renderer` bounds intersect. The `Bounds` structure also has a handy [`Intersects`][1] method which helps determine whether two bounds intersect.

`Bounds` also help us to calculate a close approximate of the *actual* (surface to surface) distance between objects (see [`Bounds.SqrDistance`][2]).

Caveats
=======

Bounds checking works really well for convex objects, but bounds checks on concave objects may lead to much higher inaccuracies depending on the shape of the object. 

Using `Mesh.bounds` is not recommended as it returns local space bounds. Use `MeshRenderer.bounds` instead. 
 


  [1]: https://docs.unity3d.com/ScriptReference/Bounds.Intersects.html
  [2]: https://docs.unity3d.com/ScriptReference/Bounds.SqrDistance.html

## Avoid empty unity methods
Avoid empty unity methods. Apart from being bad programming style, there is a very small overhead involved in runtime scripting. Over many instances, this can build up and affect performance.

<!-- language: lang-cs -->

    void Update
    {
    }
    
    void FixedUpdate
    {
    }



## Cache references
Cache references to avoid the expensive calls especially in the update function. This can be done by caching these references on start if available or when available and checking for null/bool flat to avoid getting the reference again.

Examples:

**Cache component references**

change

    void Update()
    {
        var renderer = GetComponent<Renderer>();
        renderer.material.SetColor("_Color", Color.green);
    }
    

to

    private Renderer myRenderer;
    void Start()
    {
        myRenderer = GetComponent<Renderer>();
    }

    void Update()
    {
        myRenderer.material.SetColor("_Color", Color.green);
    }
    

**Cache object references**

change

    void Update()
    {
        var enemy = GameObject.Find("enemy");
        enemy.transform.LookAt(new Vector3(0,0,0));
    }
    
to


    private Transform enemy;

    void Start()
    {
        this.enemy = GameObject.Find("enemy").transform;
    }

    void Update()
    {
        enemy.LookAt(new Vector3(0, 0, 0));
    }
    
Additionally cache expensive calls like calls to Mathf where possible.

## Avoid calling methods using strings
Avoid calling methods using strings that can accept methods. This approach will make use of reflection that can slow down your game especially when used in the update function.

**Examples:**

        //Avoid StartCoroutine with method name
        this.StartCoroutine("SampleCoroutine");

        //Instead use the method directly
        this.StartCoroutine(this.SampleCoroutine());

        //Avoid send message
        var enemy = GameObject.Find("enemy");
        enemy.SendMessage("Die");

        //Instead make direct call
        var enemy = GameObject.Find("enemy") as Enemy;
        enemy.Die();
    

