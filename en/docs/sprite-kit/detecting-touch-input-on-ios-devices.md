---
title: "Detecting touch input on iOS devices"
slug: "detecting-touch-input-on-ios-devices"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Detecting touch
You can override 4 methods of `SKScene` to detect user touch

    class GameScene: SKScene {
        
    
        override func touchesBegan(touches: Set<UITouch>, withEvent event: UIEvent?) {
    
        }
        
        override func touchesMoved(touches: Set<UITouch>, withEvent event: UIEvent?) {
            
        }
        
        override func touchesEnded(touches: Set<UITouch>, withEvent event: UIEvent?) {
            
        }
        
        override func touchesCancelled(touches: Set<UITouch>?, withEvent event: UIEvent?) {
        
        }
    }

> Please note that each method receives a `touches` parameter which (under particular circumstances) can contain more then one single touch event.


