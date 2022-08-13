---
title: "Timed functions in SpriteKit  SKActions vs NSTimers"
slug: "timed-functions-in-spritekit--skactions-vs-nstimers"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

When should you use `SKAction`s to perform timer functions?  Almost always.  The reason for this is because `Sprite Kit` operates on an update interval, and the speed of this interval can be changed throughout the life time of the process using the `speed` property.  Scenes can also be paused as well.  Since `SKAction`s work inside the scene, when you alter these properties,  there is no need to alter your time functions.  If your scene is 0.5 seconds into the process, and you pause the scene,  you do not need to stop any timers and retain that 0.5 second difference.  It is given to you automatically, so that when you unpause, the remaining time continues.

When should you use `NSTimer`s to perform timer functions?  Whenever you have something that needs to be timed outside of the `SKScene` environment, and also needs to be fired even when the scene is paused, or needs to fire at a constant rate even when the scene speed changes.

This is best used when working with both `UIKit` controls and `SpriteKit` controls.  Since `UIKit` has no idea about what goes on with `SpriteKit`, `NSTimer`s will fire regardless of the state of the `SKScene`.  An example would be we have a `UILabel` that receives an update every second, and it needs data from inside your `SKScene`.

## Implementing a method that fires after one second
**SKAction:**

`let waitForOneSecond = SKAction.waitForDuration(1)
let action = SKAction.runBlock(){action()}
let sequence = SKAction.sequence([waitForOneSecond,action])
self.runAction(sequence)`

**NSTimer:**

`NSTimer.scheduledTimerWithTimeInterval(1, target: self, selector: #selector(action), userInfo: nil, repeats: false)`

