---
title: "SKNode Collision"
slug: "sknode-collision"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The determinants of Sprite Kit collision and contact event processing are the relationship settings, created by you, of `categoryBitMask`, `collisionBitMask` and `contactTestBitMask` for each of your interacting object types. By rationally setting these in service of your desired outcomes from contacts and collisions, you determine which types can collide and inform of contacts with others, and avoid undesired collision, contact and physics processing overhead.

For each type of 'entity' you can set all three:

 1. `categoryBitMask`  : a category specific to this type of node
 2. `collisionBitMask`  : a collision differentiator, can be different from above
 3. `contactTestBitMask` : a contact differentiator, can be different from both above

The general steps to implement collisions & contacts are:

 1. set physic body size, shape and (sometimes) mass
2.  add necessary BitMasks for your node type from category, collision and contact above
 2. set scene as a contact delegate enabling it to check and inform of collisions and contacts
 3. implement contact handlers and any other pertinent logic for physics events


## Alternative to Handling contact when dealing with multi category sprites


    let bodies = (contact.bodyA.categoryBitMask <= contact.bodyB.categoryBitMask) ? (A:contact.bodyA,B:contact.bodyB) : (A:contact.bodyB,B:contact.bodyA)
        
    
    switch (bodies.A.categoryBitMask,bodies.B.categoryBitMask)
    {
      case let (a, _) where (a && superPower): //All we care about is if category a has a super power
            //do super power effect
            fallthrough //continue onto check if we hit anything else 
      case let (_, b) where (b && superPower): //All we care about is if category b has a super power
            //do super power effect
            fallthrough //continue onto check if we hit anything else 
      case let (a, b) where  (a && groundBody) && (b && boxBody): //Check if box hit ground
           //boxBody hit ground
      case let (b, _) where  (b && boxBody): //Check if box hit anything else
           //box body hit anything else 
       default:()
      
    }



## Alternative didBeginContact
IF you are using simple categories, with each physics body belonging to only one category, then this alternative form of didBeginContact may be more readable:

    func didBeginContact(contact: SKPhysicsContact) {
        let contactMask = contact.bodyA.categoryBitMask | contact.bodyB.categoryBitMask
        
    switch contactMask {
    
    case categoryBitMask.player | categoryBitMask.enemy:
        print("Collision between player and enemy")
        let enemyNode = contact.bodyA.categoryBitMask == categoryBitMask.enemy ? contact.bodyA.node! : contact.bodyB.node!
        enemyNode.explode()
        score += 10

    case categoryBitMask.enemy | categoryBitMask.enemy:
        print("Collision between enemy and enemy")
        contact.bodyA.node.explode()
        contact.bodyB.node.explode()
    
    default :
        //Some other contact has occurred
        print("Some other contact")
    }
    }

## Simple Sprite Kit project showing collisions, contacts & touch events.
Here is a simple Sprite-Kit GameScene.swift. Create a new, empty SpriteKit project and replace the GameScene.swift with this. Then build and run.

Click on any of the objects on screen to give make them move. Check the logs and the comments to see which ones collide and which ones make contact.





    //
    //  GameScene.swift
    //  bounceTest
    //
    //  Created by Stephen Ives on 05/04/2016.
    //  Copyright (c) 2016 Stephen Ives. All rights reserved.
    //
    
    import SpriteKit
    
    
    
    class GameScene: SKScene, SKPhysicsContactDelegate {
        
        let objectSize = 150
        let initialImpulse: UInt32 = 300  // Needs to be proportional to objectSize
        
        //Physics categories
        let purpleSquareCategory:   UInt32 = 1 << 0
        let redCircleCategory:      UInt32 = 1 << 1
        let blueSquareCategory:     UInt32 = 1 << 2
        let edgeCategory:           UInt32 = 1 << 31
        
        let purpleSquare = SKSpriteNode()
        let blueSquare = SKSpriteNode()
        let redCircle = SKSpriteNode()
        
        override func didMove(to view: SKView) {
            
            physicsWorld.gravity = CGVector(dx: 0, dy: 0)
            
            //Create an boundary else everything will fly off-screen
            let edge = frame.insetBy(dx: 0, dy: 0)
            physicsBody = SKPhysicsBody(edgeLoopFrom: edge)
            physicsBody?.isDynamic = false  //This won't move
            name = "Screen_edge"
            
            scene?.backgroundColor = SKColor.black
            
            //        Give our 3 objects their attributes
            
            blueSquare.color = SKColor.blue
            blueSquare.size = CGSize(width: objectSize, height: objectSize)
            blueSquare.name = "shape_blueSquare"
            blueSquare.position = CGPoint(x: size.width * -0.25, y: size.height * 0.2)
            
            let circleShape = SKShapeNode(circleOfRadius: CGFloat(objectSize))
            circleShape.fillColor = SKColor.red
            redCircle.texture = view.texture(from: circleShape)
            redCircle.size = CGSize(width: objectSize, height: objectSize)
            redCircle.name = "shape_redCircle"
            redCircle.position = CGPoint(x: size.width * 0.4, y: size.height * -0.4)
            
            purpleSquare.color = SKColor.purple
            purpleSquare.size = CGSize(width: objectSize, height: objectSize)
            purpleSquare.name = "shape_purpleSquare"
            purpleSquare.position = CGPoint(x: size.width * -0.35, y: size.height * 0.4)
            
            addChild(blueSquare)
            addChild(redCircle)
            addChild(purpleSquare)
            
            redCircle.physicsBody = SKPhysicsBody(circleOfRadius: redCircle.size.width/2)
            blueSquare.physicsBody = SKPhysicsBody(rectangleOf: blueSquare.frame.size)
            purpleSquare.physicsBody = SKPhysicsBody(rectangleOf: purpleSquare.frame.size)
            
            setUpCollisions()
            
            checkPhysics()
            
        }
        
        
        func setUpCollisions() {
            
            //Assign our category bit masks to our physics bodies
            purpleSquare.physicsBody?.categoryBitMask = purpleSquareCategory
            redCircle.physicsBody?.categoryBitMask = redCircleCategory
            blueSquare.physicsBody?.categoryBitMask = blueSquareCategory
            physicsBody?.categoryBitMask = edgeCategory  // This is the edge for the scene itself
            
            // Set up the collisions. By default, everything collides with everything.
            
            redCircle.physicsBody?.collisionBitMask &= ~purpleSquareCategory  // Circle doesn't collide with purple square
            purpleSquare.physicsBody?.collisionBitMask = 0   // purpleSquare collides with nothing
            //        purpleSquare.physicsBody?.collisionBitMask |= (redCircleCategory | blueSquareCategory)  // Add collisions with red circle and blue square
            purpleSquare.physicsBody?.collisionBitMask = (redCircleCategory)  // Add collisions with red circle
            blueSquare.physicsBody?.collisionBitMask = (redCircleCategory)  // Add collisions with red circle
            
            
            // Set up the contact notifications. By default, nothing contacts anything.
            redCircle.physicsBody?.contactTestBitMask |= purpleSquareCategory   // Notify when red circle and purple square contact
            blueSquare.physicsBody?.contactTestBitMask |= redCircleCategory     // Notify when blue square and red circle contact
            
            // Make sure everything collides with the screen edge and make everything really 'bouncy'
            enumerateChildNodes(withName: "//shape*") { node, _ in
                node.physicsBody?.collisionBitMask |= self.edgeCategory  //Add edgeCategory to the collision bit mask
                node.physicsBody?.restitution = 0.9 // Nice and bouncy...
                node.physicsBody?.linearDamping = 0.1 // Nice and bouncy...
            }
                 
            //Lastly, set ourselves as the contact delegate
            physicsWorld.contactDelegate = self
        }
        
        func didBegin(_ contact: SKPhysicsContact) {
            let contactMask = contact.bodyA.categoryBitMask | contact.bodyB.categoryBitMask
            
            switch contactMask {
            case purpleSquareCategory | blueSquareCategory:
                print("Purple square and Blue square have touched")
            case redCircleCategory | blueSquareCategory:
                print("Red circle and Blue square have touched")
            case redCircleCategory | purpleSquareCategory:
                print("Red circle and purple Square have touched")
            default: print("Unknown contact detected")
            }
        }
        
        override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
            
            for touch in touches {
                let touchedNode = selectNodeForTouch(touch.location(in: self))
                
                if let node = touchedNode {
                    node.physicsBody?.applyImpulse(CGVector(dx: CGFloat(arc4random_uniform(initialImpulse)) - CGFloat(initialImpulse/2), dy: CGFloat(arc4random_uniform(initialImpulse)) - CGFloat(initialImpulse/2)))
                    node.physicsBody?.applyTorque(CGFloat(arc4random_uniform(20)) - CGFloat(10))
                }
                
            }
        }
        
        // Return the sprite where the user touched the screen
        func selectNodeForTouch(_ touchLocation: CGPoint) -> SKSpriteNode? {
            
            let touchedNode = self.atPoint(touchLocation)
            print("Touched node is \(touchedNode.name)")
            //        let touchedColor = getPixelColorAtPoint(touchLocation)
            //        print("Touched colour is \(touchedColor)")
            
            if touchedNode is SKSpriteNode {
                return (touchedNode as! SKSpriteNode)
            } else {
                return nil
            }
        }
        
        //MARK: - Analyse the collision/contact set up.
        func checkPhysics() {
            
            // Create an array of all the nodes with physicsBodies
            var physicsNodes = [SKNode]()
            
            //Get all physics bodies
            enumerateChildNodes(withName: "//.") { node, _ in
                if let _ = node.physicsBody {
                    physicsNodes.append(node)
                } else {
                    print("\(node.name) does not have a physics body so cannot collide or be involved in contacts.")
                }
            }
            
            //For each node, check it's category against every other node's collion and contctTest bit mask
            for node in physicsNodes {
                let category = node.physicsBody!.categoryBitMask
                // Identify the node by its category if the name is blank
                let name = node.name != nil ? node.name! : "Category \(category)"
                
                let collisionMask = node.physicsBody!.collisionBitMask
                let contactMask = node.physicsBody!.contactTestBitMask
                
                // If all bits of the collisonmask set, just say it collides with everything.
                if collisionMask == UInt32.max {
                    print("\(name) collides with everything")
                }
                
                for otherNode in physicsNodes {
                if (node.physicsBody?.dynamic == false) {
                    print("This node \(name) is not dynamic")
                }
                    if (node != otherNode) && (node.physicsBody?.isDynamic == true) {
                        let otherCategory = otherNode.physicsBody!.categoryBitMask
                        // Identify the node by its category if the name is blank
                        let otherName = otherNode.name != nil ? otherNode.name! : "Category \(otherCategory)"
                        
                        // If the collisonmask and category match, they will collide
                        if ((collisionMask & otherCategory) != 0) && (collisionMask != UInt32.max) {
                            print("\(name) collides with \(otherName)")
                        }
                        // If the contactMAsk and category match, they will contact
                        if (contactMask & otherCategory) != 0 {print("\(name) notifies when contacting \(otherName)")}
                    }
                }
            }
        }
    }



## Enable Physics World
    // World physics
        self.physicsWorld.gravity         = CGVectorMake(0, -9.8);

## Enable Node to Collide
Firstly, we set node category

    let groundBody: UInt32 = 0x1 << 0
    let boxBody: UInt32 = 0x1 << 1


Then add Ground type node and Box type node. 


    let ground = SKSpriteNode(color: UIColor.cyanColor(), size: CGSizeMake(self.frame.width, 50))
    ground.position = CGPointMake(CGRectGetMidX(self.frame), 100)
    ground.physicsBody = SKPhysicsBody(rectangleOfSize: ground.size)
    ground.physicsBody?.dynamic = false
    ground.physicsBody?.categoryBitMask = groundBody
    ground.physicsBody?.collisionBitMask = boxBody
    ground.physicsBody?.contactTestBitMask = boxBody
        
    addChild(ground)

    // Add box type node

    let box = SKSpriteNode(color: UIColor.yellowColor(), size: CGSizeMake(20, 20))
    box.position = location
    box.physicsBody = SKPhysicsBody(rectangleOfSize: box.size)
    box.physicsBody?.dynamic = true
    box.physicsBody?.categoryBitMask = boxBody
    box.physicsBody?.collisionBitMask = groundBody | boxBody
    box.physicsBody?.contactTestBitMask = boxBody
    box.name = boxId
                
    let action = SKAction.rotateByAngle(CGFloat(M_PI), duration:1)
                
    box.runAction(SKAction.repeatActionForever(action))
                
    self.addChild(box)



## Handle Contacts
Set scene as delegate

    //set your scene as SKPhysicsContactDelegate

    class yourScene: SKScene, SKPhysicsContactDelegate

    self.physicsWorld.contactDelegate = self;
    
Then you have to implement one or the other of the contact functions: optional func didBegin(contact:) and/or optional fund didEnd(contact:) method to fill in your contact logic e.g. like

    //order

    let bodies = (contact.bodyA.categoryBitMask <= contact.bodyB.categoryBitMask) ? (A:contact.bodyA,B:contact.bodyB) : (A:contact.bodyB,B:contact.bodyA)
        
        
    //real handler
    if ((bodies.B.categoryBitMask & boxBody) == boxBody){
       if ((bodies.A.categoryBitMask & groundBody) == groundBody) {
           let vector = bodies.B.velocity
           bodies.B.velocity = CGVectorMake(vector.dx, vector.dy * 4)

       }else{
           let vector = bodies.A.velocity
           bodies.A.velocity = CGVectorMake(vector.dx, vector.dy * 10)

       }
    }




## Difference between contacts and collisions
In Sprite-Kit, there is the concept of **collisions** which refers to the SK physics engine handling how physics objects interact when they collide i.e. which ones bounce off which other ones.

It also has the concept of **contacts**, which is the mechanism by which your program gets informed when 2 physics objects intersect.

Objects may collide but not generate contacts, generate contacts without colliding, or collide and generate a contact (or do neither and not interact at all)

Collisions can also be one-sided i.e. object A can collide (bounce off) object B, whilst object B carries on as though nothing had happened. If you want 2 object to bounce off each other, they must both be told to collide with the other.

Contacts however are not one-sided; if you want to know when object A touched (contacted) object B, it is enough to set up contact detection on object A with regards to object B. You do not have to set up contact detection on object B for object A.

## Manipulating contactTest and collison bitmasks to enable/disable specific contact and collisions.
For this example, we will used 4 bodies and will show only the last 8 bits of the bit masks for simplicity. The 4 bodies are 3 SKSpriteNodes, each with a physics body and a boundary:

        let edge = frame.insetBy(dx: 0, dy: 0)
        physicsBody = SKPhysicsBody(edgeLoopFrom: edge)

Note that the 'edge' physics body is the physics body of the scene, not a node.

We define 4 unique categories

    let purpleSquareCategory:   UInt32 = 1 << 0  // bitmask is ...00000001
    let redCircleCategory:      UInt32 = 1 << 1  // bitmask is ...00000010
    let blueSquareCategory:     UInt32 = 1 << 2  // bitmask is ...00000100
    let edgeCategory:           UInt32 = 1 << 31  // bitmask is 10000...00000000

Each physics body is assigned the categories that it belongs to:

            //Assign our category bit masks to our physics bodies
            purpleSquare.physicsBody?.categoryBitMask = purpleSquareCategory
            redCircle.physicsBody?.categoryBitMask = redCircleCategory
            blueSquare.physicsBody?.categoryBitMask = blueSquareCategory
            physicsBody?.categoryBitMask = edgeCategory  // This is the edge for the scene itself

If a bit in a body's collisionBitMask is set to 1, then it collides (bounces off) any body that has a '1' in the same position in it's categoryBitMask. Similarly for contactTestBitMask.
 
Unless you specify otherwise, everything collides with everything else and no contacts are generated (your code won't be notified when anything contacts anything else):

    purpleSquare.physicsBody.collisonBitMask = 11111111111111111111111111111111 // 32 '1's.

Every bit in every position is '1', so when compared to any other categoryBitMask, Sprite Kit will find a '1' so a collision will occur. If you do not want this body to collide with a certain category, you will have to set the correct bit in the collisonBitMask to '0'

and its `contactTestbitMask` is set to all `0`s:

    redCircle.physicsBody.contactTestBitMask = 00000000000000000000000000000000  // 32 '0's

Same as for collisionBitMask, except reversed.

Contacts or collisions between bodies can be turned **off** (leaving existing contact or collision unchanged) using:

    nodeA.physicsBody?.collisionBitMask &= ~nodeB.category

We logically AND nodeA's collision bit mask with the inverse (logical NOT, the ~ operator) of nodeB's category bitmask to 'turn off' that bit nodeA's bitMask. e.g to stop the red circle from colliding with the purple square:

    redCircle.physicsBody?.collisionBitMask = redCircle.physicsBody?.collisionBitMask & ~purpleSquareCategory

which can be shortened to:

    redCircle.physicsBody?.collisionBitMask &= ~purpleSquareCategory

Explanation:
    
    redCircle.physicsBody.collisonBitMask = 11111111111111111111111111111111
    purpleSquareCategory  = 00000000000000000000000000000001
    ~purpleSquareCategory = 11111111111111111111111111111110 
    11111111111111111111111111111111 & 11111111111111111111111111111110 = 11111111111111111111111111111110 
    redCircle.physicsBody.collisonBitMask now equals 11111111111111111111111111111110 

redCircle no longer collides with bodies with a category of ....0001 (purpleSquare)

Instead of turning off individual bits in the collsionsbitMask, you can set it directly:

    blueSquare.physicsBody?.collisionBitMask = (redCircleCategory | purpleSquareCategory)

i.e. `blueSquare.physicsBody?.collisionBitMask = (....00000010 OR ....00000001)`

which equals `blueSquare.physicsBody?.collisionBitMask = ....00000011`

blueSquare will only collide with bodies with a category or ..01 or ..10 

Contacts or collisions between 2 bodies can be turned **ON** (without affecting any existing contacts or collisions) at any point using:

    redCircle.physicsBody?.contactTestBitMask |= purpleSquareCategory

We logically AND redCircle's bitMask with purpleSquare's category bitmask to 'turn on' that bit in redcircle's bitMask. This leaves any other bits in redCircel's bitMas unaffected.

You can make sure that every shape 'bounces off' a screen edge as follows:

    // Make sure everything collides with the screen edge
    enumerateChildNodes(withName: "//*") { node, _ in
        node.physicsBody?.collisionBitMask |= self.edgeCategory  //Add edgeCategory to the collision bit mask
    }

**Note:**

Collisions can be one-sided i.e. object A can collide (bounce off) object B, whilst object B carries on as though nothing had happened. If you want 2 object to bounce off each other, they must both be told to collide with the other:

    blueSquare.physicsBody?.collisionBitMask = redCircleCategory
    redcircle.physicsBody?.collisionBitMask = blueSquareCategory

Contacts however are not one-sided; if you want to know when object A touched (contacted) object B, it is enough to set up contact detection on object A with regards to object B. You do not have to set up contact detection on object B for object A.

    blueSquare.physicsBody?.contactTestBitMask = redCircleCategory

We don't need `redcircle.physicsBody?.contactTestBitMask= blueSquareCategory`

**Advanced usage:**

Not covered here, but physics bodies can belong to more than one category. E.g. we could set our game up as follows:

    let squareCategory:   UInt32 = 1 << 0   // bitmask is ...00000001
    let circleCategory:   UInt32 = 1 << 1   // bitmask is ...00000010
    let blueCategory:     UInt32 = 1 << 2   // bitmask is ...00000100
    let redCategory:      UInt32 = 1 << 3   // bitmask is ...00001000
    let purpleCategory:   UInt32 = 1 << 4   // bitmask is ...00010000
    let edgeCategory:     UInt32 = 1 << 31  // bitmask is 10000...0000000

Each physics body is assigned the categories that it belongs to:

            //Assign our category bit masks to our physics bodies
            purpleSquare.physicsBody?.categoryBitMask = squareCategory | purpleCategory
            redCircle.physicsBody?.categoryBitMask = circleCategory | redCategory
            blueSquare.physicsBody?.categoryBitMask = squareCategory | blueCategory

their categorybitMasks are now:

    purpleSquare.physicsBody?.categoryBitMask = ...00010001
    redCircle.physicsBody?.categoryBitMask    = ...00001010
    blueSquare.physicsBody?.categoryBitMask   = ...00000101

This will affect how you manipulate the bit fields. It can be useful (for example) to indicate that a physics body (e.g. a bomb) has changed somehow (e.g. it might have gained the 'super' ability which is another category, and you might check that a certain object (an alien mothersh


