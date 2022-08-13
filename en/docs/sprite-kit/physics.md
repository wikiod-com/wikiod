---
title: "Physics"
slug: "physics"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## How to correctly remove node in didBeginContact method (multiple contacts)
    // PHYSICS CONSTANTS
    struct PhysicsCategory {
        static let None       : UInt32 = 0
        static let All        : UInt32 = UInt32.max
        static let player     : UInt32 = 0b1             // 1
        static let bullet     : UInt32 = 0b10            // 2 
    }

    var nodesToRemove = [SKNode]()

        // #-#-#-#-#-#-#-#-#-#-#-#-#-#-#
        //MARK: - Physic Contact Delegate methods
        // #-#-#-#-#-#-#-#-#-#-#-#-#-#-#
        
        func didBegin(_ contact: SKPhysicsContact) {
            var one: SKPhysicsBody
            var two: SKPhysicsBody
            
            if contact.bodyA.categoryBitMask < contact.bodyB.categoryBitMask {
                one = contact.bodyA
                two = contact.bodyB
            } else {
                one = contact.bodyB
                two = contact.bodyA
            }
            
            // PLAYER AND BULLET
            if one.categoryBitMask == PhysicsCategory.player && two.categoryBitMask == PhysicsCategory.bullet {
               nodesToRemove.append(one.node!) // remove player
               nodesToRemove.append(two.node!) // remove bullet
            }
        }
        override func didFinishUpdate()
        {
            nodesToRemove.forEach(){$0.removeFromParent()}
            nodesToRemove = [SKNode]()
        }
    }

