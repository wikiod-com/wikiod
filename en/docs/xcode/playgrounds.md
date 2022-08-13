---
title: "Playgrounds"
slug: "playgrounds"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Getting Started with Playground
1. Create a new playground file:
    - First option: From Xcode welcome screen, select the first option (**Get started with a playground**).

    [![get started with playground][1]][1]

    - Second option: From menu select **File** → **New** → **Playground** (⌥⇧⌘N).

2. Name your playground and select the platform (iOS/macOS/tvOS), then click **Next**.

[![enter image description here][2]][2]

3. On the next screen, choose where you want to save your playground, then click **Create**.


  [1]: http://i.stack.imgur.com/rR4b4.png
  [2]: https://i.stack.imgur.com/V9vkP.png

## Latest Value, Value History and Graph
Using Playground it is easy to see that happens inside loops or objects while the change is happening.

For example, in the code below, the value of `x` will change from 1 to 4. 

    import UIKit
    for x in [1, 2, 3, 4] {
      x
    }

**(1)** Clicking on the eye symbol on the right will give us a quick look.

**(2)** Clicking on the circle next to it will open show the *Latest Value* below the line.

**(3)** Right click on the view added will show a drop down menu with **Latest Value, Value History and Graph**

[![playground][1]][1]


  [1]: http://i.stack.imgur.com/VdnB8.png

## Adding Images, Static Data, Sounds, etc. to a Playground
Images, static data, sounds, etc. are resources in a Playground.
1. If the Project Navigator is hidden, choose View > Navigators > Show Project Navigator (⌘1)
2. There are several ways to add files
     * Drag your resources to the `Resources` folder or
     * Select the Resources folder and choose File > Add Files to "Resources" or
     * control-click the Resources folder and choose Add Files to "Resources"
3. Use your resource. For example `let i = UIImage(named: "tacos.jpg")`

