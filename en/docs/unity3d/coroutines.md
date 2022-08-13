---
title: "Coroutines"
slug: "coroutines"
draft: false
images: []
weight: 9802
type: docs
toc: true
---

## Syntax
 - public Coroutine StartCoroutine(IEnumerator routine);
 - public Coroutine StartCoroutine(string methodName, object value = null);
 - public void StopCoroutine(string methodName);
 - public void StopCoroutine(IEnumerator routine);
 - public void StopAllCoroutines(); 

Performance considerations
=====

It's best to use coroutines in moderation as the flexibility comes with a performance cost.

 - Coroutines in great numbers demands more from the CPU than standard Update methods.
 - There is an issue in some versions of Unity where coroutines produce garbage each update cycle due to Unity boxing the `MoveNext` return value. This was last observed in 5.4.0b13. ([Bug report][1])


  [1]: https://issuetracker.unity3d.com/issues/coroutines-generate-garbage-in-movenext

Reduce garbage by caching YieldInstructions
---

A common trick to reduce the garbage generated in coroutines is to cache the `YieldInstruction`.

<!-- language: c# -->
    IEnumerator TickEverySecond()
    {
        var wait = new WaitForSeconds(1f); // Cache
        while(true)
        {
            yield return wait; // Reuse
        }
    }

Yielding `null` produces no extra garbage.

## Coroutines
First it's essential to understand that, game engines (such as Unity) work on a "frame based" paradigm.

Code is executed during every frame.

That includes Unity's own code, and your code.

When thinking about frames, it's important to understand that there is ***absolutely*** no guarantee of when frames happen. They ***do not*** happen on a regular beat. The gaps between frames could be, for example, 0.02632 then 0.021167 then 0.029778, and so on. In the example they are all "about" 1/50th of a second, but they are all different. And at any time, you may get a frame that takes much longer, or shorter; and your code may be executed at any time at all within the frame.

Bearing that in mind, you may ask: how do you access these frames in your code, in Unity?

Quite simply, you use either the Update() call, or, you use a coroutine. (Indeed - they are exactly the same thing: they allow code to be run every frame.)

The purpose of a coroutine is that:

you can run some code, and then, "stop and wait" ***until some future frame***.

You can wait until **the next frame**, you can wait for **a number of frames**, or you can wait for some ***approximate*** time in seconds in the future.

For example, you can wait for "about one second", meaning it will wait for about one second, and then put your code in some frame roughly one second from now. (And indeed, within that frame, the code could be run at any time, whatsoever.) To repeat: it will not be exactly one second. Accurate timing is meaningless in a game engine.

Inside a coroutine:

To wait one frame:

    // do something
    yield return null;  // wait until next frame
    // do something

To wait three frames:

    // do something
    yield return null;  // wait until three frames from now
    yield return null;
    yield return null;
    // do something

To wait **approximately** half a second:

    // do something
    yield return new WaitForSeconds (0.5f); // wait for a frame in about .5 seconds
    // do something

Do something every single frame:

    while (true)
    {
        // do something
        yield return null;  // wait until the next frame
    }

That example is literally identical to simply putting something inside Unity's "Update" call: the code at "do something" is run every frame.

Example
=======

Attach Ticker to a `GameObject`. While that game object is active, the tick will run. Note that the script carefully stops the coroutine, when the game object becomes inactive; this is usually an important aspect of correctly engineering coroutine usage.

<!-- language-all: c# -->

    using UnityEngine;
    using System.Collections;
    
    public class Ticker:MonoBehaviour {
    
        void OnEnable()
        {
            StartCoroutine(TickEverySecond());
        }

        void OnDisable()
        {
            StopAllCoroutines();
        }
    
        IEnumerator TickEverySecond()
        {
            var wait = new WaitForSeconds(1f); // REMEMBER: IT IS ONLY APPROXIMATE
            while(true)
            {
                Debug.Log("Tick");
                yield return wait;  // wait for a frame, about 1 second from now
            }
        }
    }
    

## Chaining coroutines
Coroutines can yield inside themselves, and wait for **other coroutines**. 

So, you can chain sequences - "one after the other".

This is very easy, and is a basic, core, technique in Unity.

It's absolutely natural in games that certain things have to happen "in order". Almost every "round" of a game starts with a certain series of events happening, over a space of time, in some order. Here's how you might start a car race game:

    IEnumerator BeginRace()
    {
      yield return StartCoroutine(PrepareRace());
      yield return StartCoroutine(Countdown());
      yield return StartCoroutine(StartRace());
    }

So, when you call BeginRace ...

     StartCoroutine(BeginRace());

It will run your "prepare race" routine. (Perhaps, flashing some lights and running some crowd noise, resetting scores and so on.) When that is finished, it will run your "countdown" sequence, where you would animate perhaps a countdown on the UI. When that is finished, it will run your race-starting code, where you would perhaps run sound effects, start some AI drivers, move the camera in a certain way, and so on.

For clarity, understand that the three calls

      yield return StartCoroutine(PrepareRace());
      yield return StartCoroutine(Countdown());
      yield return StartCoroutine(StartRace());

must themselves **be in** a coroutine. That is to say, they must be in a function of the type `IEnumerator`. So in our example that's `IEnumerator BeginRace`. So, from "normal" code, you launch that coroutine with the `StartCoroutine` call.

     StartCoroutine(BeginRace());

To further understand chaining, here's a function which chains coroutines. You pass in an array of coroutines. The function runs as many coroutines as you pass, in order, one after the other.

    // run various routines, one after the other
    IEnumerator OneAfterTheOther( params IEnumerator[] routines ) 
    {
        foreach ( var item in routines ) 
        {
            while ( item.MoveNext() ) yield return item.Current;
        }
    
        yield break;
    }

Here's how you would call that...let's say you have three functions. Recall they must all be `IEnumerator`:

    IEnumerator PrepareRace() 
    {
        // codesay, crowd cheering and camera pan around the stadium
        yield break;
    }
    
    IEnumerator Countdown() 
    {
        // codesay, animate your countdown on UI
        yield break;
    }
    
    IEnumerator StartRace() 
    {
        // codesay, camera moves and light changes and launch the AIs
        yield break;
    }

You'd call it like this

    StartCoroutine( MultipleRoutines( PrepareRace(), Countdown(), StartRace() ) );

or perhaps like this

    IEnumerator[] routines = new IEnumerator[] {
         PrepareRace(),
         Countdown(),
         StartRace() };
    StartCoroutine( MultipleRoutines( routines ) );

To repeat, one of the most basic requirements in games is that certain things happen one after the other "in a sequence" over time. You achieve that in Unity very simply, with

      yield return StartCoroutine(PrepareRace());
      yield return StartCoroutine(Countdown());
      yield return StartCoroutine(StartRace());



## Ending a coroutine
Often you design coroutines to naturally end when certain goals are met.

<!-- language: c# -->
    IEnumerator TickFiveSeconds()
    {
        var wait = new WaitForSeconds(1f);
        int counter = 1;
        while(counter < 5)
        {
            Debug.Log("Tick");
            counter++;
            yield return wait;
        }
        Debug.Log("I am done ticking");
    }

To stop a coroutine from "inside" the coroutine, you cannot simply "return" as you would to leave early from an ordinary function. Instead, you use `yield break`.

<!-- language: c# -->
    IEnumerator ShowExplosions()
    {
        ... show basic explosions
        if(player.xp < 100) yield break;
        ... show fancy explosions
    }

You can also force all coroutines launched by the script to halt before finishing.

<!-- language: c# -->
    void OnDisable()
    {
        // Stops all running coroutines
        StopAllCoroutines();
    }

The method to stop a _specific_ coroutine from the caller varies depending on how you started it.

If you started a coroutine by string name:

    StartCoroutine("YourAnimation");

then you can stop it by calling [StopCoroutine](https://docs.unity3d.com/ScriptReference/MonoBehaviour.StopCoroutine.html) with the same string name:

    StopCoroutine("YourAnimation");

Alternatively, you can keep a reference to _either_ the `IEnumerator` returned by the coroutine method, _or_ the `Coroutine` object returned by `StartCoroutine`, and call `StopCoroutine` on either of those:

<!-- language: c# -->
    public class SomeComponent : MonoBehaviour 
    {
        Coroutine routine;

        void Start () {
            routine = StartCoroutine(YourAnimation());
        }

        void Update () {
            // later, in response to some input...
            StopCoroutine(routine);
        }

        IEnumerator YourAnimation () { /* ... */ }
    }

## MonoBehaviour methods that can be Coroutines
There are three MonoBehaviour methods that can be made coroutines.

 1. Start()
 2. OnBecameVisible()
 3. OnLevelWasLoaded()

This can be used to create, for example, scripts that execute only when the object is visible to a camera.

<!-- language: c# -->
    using UnityEngine;
    using System.Collections;
    
    public class RotateObject : MonoBehaviour
    {
        IEnumerator OnBecameVisible()
        {
            var tr = GetComponent<Transform>();
            while (true)
            {
                tr.Rotate(new Vector3(0, 180f * Time.deltaTime));
                yield return null;
            }
        }
        
        void OnBecameInvisible()
        {
            StopAllCoroutines();
        }
    }

## Ways to yield
You can wait until the next frame.

    yield return null; // wait until sometime in the next frame

You can have multiple of these calls in a row, to simply wait for as many frames as desired.

    //wait for a few frames
    yield return null;
    yield return null;

Wait for ***approximately*** n seconds. It is extremely important to understand this is only **a very approximate time**.

    yield return new WaitForSeconds(n);

It is absolutely not possible to use the "WaitForSeconds" call for any form of accurate timing.

Often you want to chain actions.  So, do something, and when that is finished do something else, and when that is finished do something else. To achieve that, wait for another coroutine:

    yield return StartCoroutine(coroutine);

Understand that you can only call that from within a coroutine.  So:

    StartCoroutine(Test());

That's how you start a coroutine from a "normal" piece of code.

Then, inside that running coroutine:

    Debug.Log("A");
    StartCoroutine(LongProcess());
    Debug.Log("B");

That will print A, start the long process, and **immediately print B**.  It will **not** wait for the long process to finish. On the other hand:

    Debug.Log("A");
    yield return StartCoroutine(LongProcess());
    Debug.Log("B");

That will print A, start the long process, **wait until it is finished**, and then print B.

It's always worth remembering that coroutines have absolutely no connection, in any way, to threading. With this code:

    Debug.Log("A");
    StartCoroutine(LongProcess());
    Debug.Log("B");

it is easy to think of it as being "like" starting the LongProcess on another thread in the background. But that is absolutely incorrect. It is just a coroutine. Game engines are frame based, and "coroutines" in Unity simply allow you to access the frames.

It is very easy to wait for a web request to complete.

    void Start() {
        string url = "http://google.com";
        WWW www = new WWW(url);
        StartCoroutine(WaitForRequest(www));
    }

    IEnumerator WaitForRequest(WWW www) {
        yield return www;
        
        if (www.error == null) {
            //use www.data);
        }
        else {
            //use www.error);
        }
    }

For completeness: In very rare cases you use fixed update in Unity; there is a `WaitForFixedUpdate()` call which normally would never be used. There is a specific call (`WaitForEndOfFrame()` in the current version of Unity) which is used in certain situations in relation to generating screen captures during development. (The exact mechanism changes slightly as Unity evolves, so google for the latest info if relevant.)

