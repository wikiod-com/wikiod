---
title: "Navigation"
slug: "navigation"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

> # Important Note
> Apple has highly discouraged use of both navigation styles in one controller, and this may result in an app rejection.

Currently, the preferred way is to use hierarchical style rather than page based, as used in many more Apple apps than before.

## Page-based navigation
Many watchOS apps (like Activity) have several pages which you could simply scroll between them, which is a very good way to use Apple Watch.

To create a page based navigation, Ctrl-Drag from one controller to another, and select "next page", as shown in the following picture:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/7VNjE.png

## Hierarchical view structure
Many watchOS apps (like Workout, Weather, Music, etc) have a main `WKInterfaceTable` or a set of buttons which are hooked up to another controller, similar to the navigation on iOS. This is called hierarchical view structure.

To connect a button, Ctrl-Drag from the button to a controller, and select "push" or "modal" based on your need, as shown in the following picture:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/ZeSbm.png

