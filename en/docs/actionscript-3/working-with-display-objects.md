---
title: "Working With Display Objects"
slug: "working-with-display-objects"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
1. `addChild(child)` - adds a new item to this object's child tree as the topmost element.
2. `addChildAt(child, index)` - adds a new item to this object's child tree at a specified position. The bottom-most item has index of 0.
3. `getChildAt(index)` - returns a child with given index.
4. `getChildIndex(child)` returns the index of a *direct* child of this object. Otherwise an exception is thrown.
5. `removeChild(child)` - removes the specified direct child from this object's child tree. Throws exception if the supplied child's parent is not equal to `this`.
6. `removeChildAt(index)` - removes a child selected by index instead of reference. Throws exception if the child tree is not this wide.
7. `removeChildren(beginIndex:int = 0, endIndex:int = 0x7fffffff))` - added in Flash Player 11, removes a subset of children by index range, or all children if called with no parameters.
8. `setChildIndex(child,index)` - changes the child's index to the new value, shifting all children in between to occupy the released spot.
9. `swapChildren(child1,child2)` - swaps the two children's positions in display list, not affecting positions of other children.
10. `swapChildrenAt(index1,index2)` - swaps children located by their indexes. 

The display list is actually a tree, and is visualized with depth first algorithm. Any object listed earlier will be displayed earlier, and might be obscured by objects listed later. All techniques that can be used against a tree can be applied to working with display list.

## Remove all objects from the display list
If targeting Flash Player 11+, the built-in [removeChildren][1] method is the best way to remove all children:

    removeChildren(); //a start and end index can be passed 

For legacy applications, the same can be accomplished with a loop:

    while (numChildren > 0) {
        removeChildAt(0);
    }


  [1]: http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/DisplayObjectContainer.html#removeChildren%28%29

## Introduction To The Display List
In AS3, display assets are not visible until they are added to the Display List. 

The AIR/Flash runtime has a hierarchical display structure (parent child relationship where children can have their own children), with the `stage` being the top level parent.  

To add something to the display list, you use `addChild` or `addChildAt`. Here is a basic example of drawing a circle and adding it to the display list:

    var myCircle:Shape = new Shape();
            
    myCircle.graphics.beginFill(0xFF0000); //red
    myCircle.graphics.drawCircle(25, 25, 50);
    myCircle.graphics.endFill();
            
    this.addChild(myCircle); //add the circle as a child of `this`

To see the object in the example above, `this` (the context of the code) also must be on the display list, as well any parents it may have.  In AS3, the `stage` is the top most parent.

>Display objects can only have one parent.  So if a child already has a parent, and you add it to another object, it will be removed from it's previous parent.

Z-Order / Layering
------------------

Let's say you replicated the code from the previous example so you had 3 circles:

    var redCircle:Shape = new Shape();
    redCircle.graphics.beginFill(0xFF0000); //red
    redCircle.graphics.drawCircle(50, 50, 50); //graphics.endFill is not required

    var greenCircle:Shape = new Shape();
    greenCircle.graphics.beginFill(0x00FF00); //green
    greenCircle.graphics.drawCircle(75, 75, 50);
            
    var blueCircle:Shape = new Shape();
    blueCircle.graphics.beginFill(0x0000FF); //blue
    blueCircle.graphics.drawCircle(100, 100, 50);

    this.addChild(redCircle); 
    this.addChild(greenCircle); 
    this.addChild(blueCircle); 

Since the `addChild` method adds the child on top of everything else in the same parent, you'll get this result with the items layered in the same order you use addChild:

[![enter image description here][1]][1]

If you wanted a child layered different relative to it's siblings, you can use `addChildAt`.  With `addChildAt`, you pass in another parameter that indicates the index (z-order) the child should be at.  `0` being the bottom most position/layer.   
    
    this.addChild(redCircle); 
    this.addChild(greenCircle); 
    this.addChildAt(blueCircle,0); //This will add the blue circle at the bottom

[![enter image description here][2]][2]

Now the blue circle is under it's siblings. If later on, you want to change the index of a child, you can use the `setChildIndex` method (on the child's parent).

    this.setChildIndex(redCircle, this.numChildren - 1); //since z-index is 0 based, the top most position is amount of children less 1.

This will rearrange the red circle so it's above everything else.  The code above produces the exact same result as `this.addChild(redCircle)`.

Removing Display Objects
------------------------

To remove objects, you have the converse `removeChild` and `removeChildAt` methods as well as the `removeChildren` method.

    removeChild(redCircle); //this will take redCircle off the display list

    removeChildAt(0); //this will take the bottom most object off the display list 

    removeChildren(); //this will clear all children from the display list

    removeChildren(1); //this would remove all children except the bottom most

    removeChildren(1,3); //this would remove the children at indexes 1, 2 & 3

Events
------

When a child is added to the display list, some events are fired on that child.

- `Event.ADDED`
- `Event.ADDED_TO_STAGE`

Conversely, there are also the remove events:

- `Event.REMOVED`
- `Event.REMOVED_FROM_STAGE`


Adobe Animate / Flash Professional
----------------------------------

When dealing with FlashProfessional/Adobe Animate timelines, adding something to the timeline handles the display list nuances automatically.  They added and removed from the display list automatically by the timeline.

However, it's good to keep in mind that:

>If you manipulate through code the parentage of a display object created by the timeline (by using addChild/setChildIndex), that child will no longer be removed automatically by the timeline and will need to be removed via code.



  [1]: http://i.stack.imgur.com/VqKme.jpg
  [2]: http://i.stack.imgur.com/8GtkC.jpg

## Layering
There can be situations when you decide that one set of display objects should always be above another set of objects, for example, arrows over heads, explosions over something that just exploded, etc. To perform this as simple as possible, you need to designate and create a set of `Sprite`s, arrange them in order from bottom to top, then just add all objects of "above" set to a layer above the one used for objects of "below" set.

    var monsters:Vector.<Monster>;
    var bullets:Vector.<Bullet>; // desired: bullets strictly above monsters
    var monsterLayer:Sprite=new Sprite();
    var bulletLayer:Sprite=new Sprite();
    addChild(monsterLayer);
    addChild(bulletLayer);

Then, whenever you add a `Monster` to the display list, add it to `monsterLayer`, and whenever you add a `Bullet`, add to `bulletLayer` to achieve the desired effect.

## Transiting from frames to manual content switching
Early on a Flash developer uses frames, as they are natively available in Flash player, to host various screens of their application (most often it's a game). Eventually they might stumble upon an issue that something goes wrong exactly because they have used frames, and overlooked the difficulties that arise from this, and seek ways to both retain their frame structure but also remove the obstacle of using frames with its complications. The solution is to use `Sprite` descendant classes, or exported frames as `MovieClip`s with a single frame (to those that design in Adobe Flash CS), and manually switch the contents with `addChild()` and `removeChild()`. 

The manager class should have all of its child frame classes ready, and whenever a transition is called, a function similar to this can be used:

    var frames:Vector.<DisplayObject>; // this holds instances to ALL children
    var currentFrame_alt:int; // current frame. Can't use the property
    function changeFrame(frame:int):void {
        removeChild(frames[currentFrame_alt]);
        addChild(frames[frame]);
        currentFrame_alt=frame;
    }

All children can both dispatch and listen to events with `Event.ADDED_TO_STAGE` used as an entry point for whatever happens after `gotoAndStop()` targetting that frame, and any outgoing transitions can be coded as events based on strings, which are being listened in `Main` class, which then performs the transition.

    frames[0].addEventListener("startGame",startGame); // assuming frame 0 is a "Play" button
    function startGame(e:Event):void {
        changeFrame(1); // switch to frame 1 - will display frames[1]
    }

Of course, the set of strings should be predefined, for example, intro screen could have two buttons to start the game, "Start game" and "Start muted" for example, and the buttons should dispatch different events, which will then be handled differently in manager class.

This pattern can go as deep as you need to. If any frame of the project contains a MovieClip with multiple frames, it can also be decoupled into sprites with this method.

