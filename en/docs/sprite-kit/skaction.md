---
title: "SKAction"
slug: "skaction"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Create and Run a Simple SKAction
A very simple example would be to fade out an SKSpriteNode.

**In Swift:**

    let node = SKSpriteNode(imageNamed: "image")
    let action = SKAction.fadeOutWithDuration(1.0)
    node.runAction(action)

## Creating a Repeating Sequence of Actions
Sometimes it is necessary to do an action on repeat or in a sequence. This example will make the node fade in and out a total of 3 times. 

**In Swift:**

    let node = SKSpriteNode(imageNamed: "image")
    let actionFadeOut = SKAction.fadeOutWithDuration(1.0)
    let actionFadeIn = SKAction.fadeInWithDuration(1.0)
    let actionSequence = SKAction.sequence([actionFadeOut, actionFadeIn])
    let actionRepeat = SKAction.repeatAction(actionSequence, count: 3)
    node.runAction(actionRepeat)


## Running a Block of Code in an SKAction
One helpful case is to have the action run a block of code. 

**In Swift:**

    let node = SKSpriteNode(imageNamed: "image")
    let actionBlock = SKAction.runBlock({
        //Do what you want here
        if let gameScene = node.scene as? GameScene {
            gameScene.score += 5
        }
    })
    node.runAction(actionBlock)



## Named actions that can be started or removed from elsewhere.
Sometimes you would want to start or remove an action on a specific node at a certain time. For example, you might want to stop a moving object when the user taps the screen. This becomes very helpful when a node has multiple actions and you only wants to access one of them.

    let move = SKAction.moveTo(x: 200, duration: 2)
    object.run(move, withKey: "moveX")
 
Here we set the key "moveX" for the action `move` in order to access it later in another part of the class.

      override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
            object.removeAction(forKey: "moveX")
        }
When the user touches the screen the action will get removed and the object will stop moving.

