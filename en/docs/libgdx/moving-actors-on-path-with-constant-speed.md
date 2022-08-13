---
title: "Moving actors on path with constant speed"
slug: "moving-actors-on-path-with-constant-speed"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Simple movement between two locations
For this the best solution is using `actions`. To add a new action to an actors in `Scene2D` just call:
<!-- language: java -->
    Action action = Actions.moveTo(x,y,duration);
    actorObject.addAction(action);

Where x and y is the target location and duration is the speed of this movement in seconds(`float`).

If you want to stop this action(and the actor by it) you can do it by calling:
<!-- language: java -->
    actorObject.removeAction(action);

or you can remove all actions by calling:
<!-- language: java -->
    actorObject.clearActions();

This will immediately stop the running of the action(s).

The moveTo action manipulates the x and y property of the actor, so when you draw the actor to the screen always use getX() and getY() to draw textures. Just like in the following example:

<!-- language: java -->
    public class MovingActor extends Actor {
    
        private Action runningAction;
        private float speed = 2f;
    
        public void moveTo(Vector2 location) {
           runningAction = Actions.moveTo(location.x, location.y, speed);
           this.addAction(runningAction);
        }
    
        public void stopAction() {
           this.removeAction(runningAction);
        }
    
        public void stopAllActions() {
           this.clearActions();
        }
    
        @Override
        public void draw(Batch batch, float parentAlpha){
            batch.draw(someTexture, getX(), getY());
        }
    }




