---
title: "Working with Timeline"
slug: "working-with-timeline"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Referencing the main timeline or document class from within other MovieClips
In the timeline of any `DisplayObject` that is attached as a descendant of the display tree, you can utilise the [`root`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/DisplayObject.html#root) property. This property points to the main timeline in the case of no custom document class, or the document class if you do define one.

Because `root` is typed `DisplayObject`, the compiler will not allow you to access custom methods or properties defined on the main timeline or within your document class as:

    root.myCustomProperty = 10;
    root.myCustomMethod();

To get around this, you can typecast `root` to your document class in the case where you have a document class:

    (root as MyDocumentClass).myCustomMethod();

Or `MovieClip` in the case of no document class:

    (root as MovieClip).myCustomMethod();

The reason casting to `MovieClip` works here is because `MovieClip` is [`dynamic`](http://help.adobe.com/en_US/ActionScript/3.0_ProgrammingAS3/WS5b3ccc516d4fbf351e63e3d118a9b90204-7f89.html). This means that the compiler allows runtime properties and method to be declared on it, preventing compile-time errors when attempting to access properties or methods that are not explicitly defined on `MovieClip`. The downside to this is that you lose all compile-time type safety. You are much better off declaring a document class and casting to that.

