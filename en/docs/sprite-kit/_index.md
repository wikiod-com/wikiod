---
title : sprite-kit Tutorial
slug : sprite-kit-tutorial
weight : 9970
draft : false
images : []
type : docs
---

SpriteKit is a 2D Game Engine developed by Apple. It provides high level APIs and a wide range of functionalities to developers. It also contains an internal Physics Engine.

It is available on every Apple platform

 - iOS
 - macOS
 - tvOS
 - watchOS (>= 3.0)

Note: If you wish to develop using 3D graphics you need to use SceneKit instead.

The core building blocks of SpriteKit are:
- [SKView][1]: a view in which SKScenes are presented.
- [SKScene][2]: a 2D scene that is presented in an SKView and contains one or more SKSpriteNodes.
- [SKSpriteNode][3]: an individual 2D image that can be animated around the scene.

Other related building blocks are:
- SKNode: a more general node that can be used in a scene to group other nodes together for more complex behaviour.
- SKAction: single or groups of actions that are applied to SKNodes to implement animations and other effects.
- SKPhysicsBody - allows physics to be applied to SKNodes to allow them to behave in a realistic manner, including falling under gravity, bouncing off each other and following ballistic trajectories.

[Official documentation][4].


  [1]: https://www.wikiod.com/sprite-kit/skview
  [2]: https://www.wikiod.com/sprite-kit/skscene
  [3]: https://www.wikiod.com/sprite-kit/skspritenode-sprites
  [4]: https://developer.apple.com/library/ios/documentation/GraphicsAnimation/Conceptual/SpriteKit_PG/Introduction/Introduction.html

