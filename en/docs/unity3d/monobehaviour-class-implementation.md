---
title: "MonoBehaviour class implementation"
slug: "monobehaviour-class-implementation"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## No overridden methods
The reason you do not have to override `Awake`, `Start`, `Update` and other method is because they are not virtual methods defined in a base class.

The first time your script gets accessed, the scripting runtime looks through the script to see if some methods are defined. If they are, that information is cached and the methods are added to their respective list. These lists are then simply looped through at different times.

The reason these methods are not virtual is because of performance. If all the scripts would have `Awake`, `Start`, `OnEnable`, `OnDisable`, `Update`, `LateUpdate`, and `FixedUpdate`, then these would all be added to their lists what would mean that all these methods get executed. Normally this wouldn't be a big problem, however, all these method calls are from the native side (C++) to the managed side (C#) which comes with a performance cost.

Now imagine this, all these methods are in their lists and some/most of them might not even have an actual method body. This would mean that there is a huge amount of performance wasted on calling methods that do not even do anything. To prevent this, Unity opted out of using virtual methods and made a messaging system that makes sure that these methods only get called when they are actually defined, saving unnecessary method calls.

You can read more on the matter on an Unity blog over here: [10000 Update() Calls][1] and more on IL2CPP over here: [An Introduction to IL2CPP Internals][2] 


  [1]: http://blogs.unity3d.com/2015/12/23/1k-update-calls/
  [2]: http://blogs.unity3d.com/2015/05/06/an-introduction-to-ilcpp-internals/

