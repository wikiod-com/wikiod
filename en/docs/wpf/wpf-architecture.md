---
title: "WPF Architecture"
slug: "wpf-architecture"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## DispatcherObject
Derives from
------------
`Object`

Key members
-----------

    public Dispatcher Dispatcher { get; }

Summary
-------
Most objects in WPF derive from `DispatcherObject`, which provides the basic constructs for dealing with concurrency and threading. Such objects are associated with a Dispatcher.

Only the thread that the Dispatcher was created on may access the DispatcherObject directly. To access a DispatcherObject from a thread other than the thread the DispatcherObject was created on, a call to `Invoke` or `BeginInvoke` on the Dispatcher the object is associated with is required.

## DependencyObject
Derives from
------------
`DispatcherObject`

Key members
-----------

    public object GetValue(DependencyProperty dp);
    public void   SetValue(DependencyProperty dp, object value);

Summary
-------
Classes derived from `DependencyObject` participate in the [dependency property][1] system, which includes registering dependency properties and providing identification and information about such properties. Since dependency properties are the cornerstone of WPF development, all WPF controls ultimately derive from `DependencyObject`.

  [1]: https://www.wikiod.com/wpf/dependency-properties

