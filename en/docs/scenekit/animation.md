---
title: "Animation"
slug: "animation"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Load an animation from a collada or scn file
**Swift**

Function for loading an animation from a file:

    func animationFromSceneNamed(path: String) -> CAAnimation? {
        let scene  = SCNScene(named: path)
        var animation:CAAnimation?
        scene?.rootNode.enumerateChildNodes({ child, stop in
            if let animKey = child.animationKeys.first {
                animation = child.animation(forKey: animKey)
                stop.pointee = true
            }
        })
        return animation
    }

Apply animation to node:

    let animation = animationFromSceneNamed("art.scnassets/animation.dae")
    myNode.addAnimation(animation, forKey: "anim")

