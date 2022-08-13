---
title: "SKSpriteNode (Sprites)"
slug: "skspritenode-sprites"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- convenience init(imageNamed name: String) // Create an SKSpriteNode from a named image in the assets catalogue

- var position: CGPoint // SKNode property, inherited by SKSpriteNode. The position of the node in the parents co-ordinate system.

- func addChild(_ node: SKNode) // SKNode method, inherited by SKScene. Used to add an SKSpriteNode to the scene (also used to add SKNodes to other SKNodes).




## Subclassing SKSpriteNode
You can subclass `SKSpriteNode` and define your own type of sprite.

    class Hero: SKSpriteNode {
        //Use a convenience init when you want to hard code values
        convenience init() {
            let texture = SKTexture(imageNamed: "Hero")
            self.init(texture: texture, color: .clearColor(), size: texture.size())
        }

        //We need to override this to allow for class to work in SpriteKit Scene Builder
        required init?(coder aDecoder: NSCoder) {
            super.init(coder:aDecoder)
        }

        //Override this to allow Hero to have access all convenience init methods
        override init(texture: SKTexture?, color: UIColor, size: CGSize) 
        {
            super.init(texture: texture, color: color, size: size)
        }
    }

## Adding a Sprite to the Scene
In SpriteKit a Sprite is represented by the `SKSpriteNode` class (which inherits from `SKNode`).

First of all create a new Xcode Project based on the SpriteKit template as described in [Your First SpriteKit Game][1].

Creating a Sprite
-
Now you can create a SKSpriteNode using an image loaded into the `Assets.xcassets` folder.

    let spaceship = SKSpriteNode(imageNamed: "Spaceship")

> `Spaceship` is the name of the image item into the Assets.xcassets.

After the sprite has been created you can add it to your scene (or to any other node).

Open `GameScene.swift`, remove all its content and add the following

    class GameScene: SKScene {
        override func didMoveToView(view: SKView) {
            let enemy = SKSpriteNode(imageNamed: "Spaceship")
            enemy.position = CGPoint(x:self.frame.midX, y:self.frame.midY)
            self.addChild(enemy)
        }
    }

Now press <kbd>CMD</kbd> + <kbd>R</kbd> in Xcode to launch the Simulator.

[![enter image description here][2]][2]


  [1]: https://www.wikiod.com/sprite-kit/getting-started-with-sprite-kit#Your first SpriteKit Game (Hello World)
  [2]: http://i.stack.imgur.com/MWDjs.png

