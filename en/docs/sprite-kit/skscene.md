---
title: "SKScene"
slug: "skscene"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

SKScene represents a single scene in a SpriteKit application. An SKScene is 'presented' into an [SKView][1]. [SKSpriteNodes][2] are added to the scene to implement the actual sprites.

Simple applications may have a single SKScene that contains all the SpriteKit content. More complex apps may have several SKScenes that are presented at different times (e.g. an opening scene to present the game options, a second scene to implement the game itself and a third scene to present the 'Game Over' results).


  [1]: https://www.wikiod.com/sprite-kit/skview
  [2]: https://www.wikiod.com/sprite-kit/skspritenode-sprites

## Create an SKScene that Fills the SKView
A simple use case it to create an SKScene that exactly fills the SKView. This avoids the need to consider scaling the view to fit or setting a camera to show a part of the scene.

The following code assumes an SKView called skView already exists (e.g. as defined in [Create a Full Screen SKView using Interface Builder][1]) and a subclass of SKScene called GameView has been defined:

**In Swift**: 

    let sceneSize = CGSizeMake(skView.frame.width, skView.frame.height)
    let scene = SKScene(size: sceneSize)
    
    skView.presentScene(scene)

However if the SKView can change size (e.g. if the user rotates their device and this causes the view to be stretched because of its constraints) then the SKScene will no longer fit the SKView. You could manage this by resizing the SKScene each time the SKView changes size (e.g. in the didChangeSize method).

  [1]: https://www.wikiod.com/sprite-kit/skview#Create a full screen SKView using Interface Builder

## Create an SKScene that Scales to fit the SKView
An SKScene has a **scaleMode** parameter that defines how it will change its size to fit within the SKView it is presented into the SKView if it is not the same size and/or shape.

There are four options for scaleMode:

 - **AspectFit**: the scene is scaled (but not stretched) until it fits within the view. This ensures that the scene is not distorted but there may be some areas of the view that are not covered by the scene if the scene is not the same shape as the view.
 - **AspectFill**: the scene is scaled (but not stretched) to fill the view completely. This ensures that the scene is not distorted and that the view is completely filled but some parts of the scene may be cropped if the scene is not the same shape as the view.
 - **Fill**: the scene is scaled (and if necessary stretched) to fill the view completely. This ensure that the view is completely filled and that none of your scene is cropped but the scene will be distorted if the scene is not the same shape as the view.
 - **ResizeFill**: the scene is not scaled at all but rather its size is changed to fit the size of the view.

The following code assumes an SKView called skView already exists (e.g. as defined in [Create a Full Screen SKView using Interface Builder][1]) and a subclass of SKScene called GameView has been defined and then uses the **AspectFill** scaleMode:

**In Swift 3**: 

        let sceneSize = CGSize(width:1000, height:1000)
        let scene = GameScene(size: sceneSize)
        scene.scaleMode = .aspectFill

        skView.presentScene(scene)


  [1]: https://www.wikiod.com/sprite-kit/skview#Create a full screen SKView using Interface Builder

## Create an SKScene with an SKCameraNode (iOS 9 and later)
You can place an SKCameraNode into an SKScene to define which part of the scene is shown in the SKView. Think of the SKScene as a 2D world with a camera floating above it: the SKView will show what the camera 'sees'.

E.g. the camera could be attached to the main character's sprite to follow the action of a scrolling game.

The SKCameraNode has four parameters that define what part of the scene is shown:

 - **position**: this is the position of the camera in the scene. The scene is rendered to place this position in the middle of the SKView.
 - **xScale** and **yScale**: these define how the scene is zoomed in the view. Keep these two values the same to avoid distorting the view. A value of 1 means no zoom, values less than one will zoom in (make the sprites appear larger) and values above 1 will zoom out (make the sprites appear smaller).
 - **zRotation**: this defines how the view is rotated in the view. A value of zero will be no rotation. The value is in radians, so a value of Pi (3.14...) will rotate the view upside-down.

The following code assumes an SKView called skView already exists (e.g. as defined in [Create a Full Screen SKView using Interface Builder][1]) and a subclass of SKScene called GameView has been defined. This example just sets the camera's initial position, you would need to move the camera (in the same way as you would other SKSpriteNodes in the scene) to scroll your view:

**In Swift 3**: 

        let sceneSize = CGSize(width:1000, height:1000)
        let scene = GameScene(size: sceneSize)
        scene.scaleMode = .aspectFill

        let camera = SKCameraNode()
        camera.position = CGPointM(x:500, y:500)
        camera.xScale = 1
        camera.yScale = 1
        camera.zRotation = 3.14
        scene.addChild(camera)
        scene.camera = camera
 
        skView.presentScene(scene)


  [1]: https://www.wikiod.com/sprite-kit/skview#Create a full screen SKView using Interface Builder

## Subclassing SKScene to Implement Primary SpriteKit Functionality
SpriteKit functionality can be implemented in a subclass of SKScene. For example, a game may implement the main game functionality within an SKScene subclass called GameScene.

**In Swift**: 

    import SpriteKit

    class GameScene: SKScene {

        override func didMoveToView(view: SKView) {
            /* Code here to setup the scene when it is first shown. E.g. add sprites. */
        }
    
        override func touchesBegan(touches: Set<UITouch>, withEvent event: UIEvent?) {
            for touch in touches {
                let location = touch.locationInNode(self)
                /* Code here to respond to a user touch in the scene at location */
            }
        }
   
        override func update(currentTime: CFTimeInterval) {
            /* Code here to perform operations before each frame is updated */
        }
    }

Secondary functionality could then be implemented in subclasses of the SKSpriteNodes that are used within the scene (see [Subclassing SKSpriteNode][1]).

  [1]: https://www.wikiod.com/sprite-kit/skspritenode-sprites#Subclassing SKSpriteNode

