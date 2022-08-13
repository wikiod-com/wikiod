---
title: "Understanding the Error 1009 Cannot access a property or method of a null object reference"
slug: "understanding-the-error-1009-cannot-access-a-property-or-method-of-a-null-object-reference"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

An error 1009 is a general error that arises when you are trying to receive a value out of a variable or property that has a value of `null`. The examples provided expose various cases where this error arises, together with some recommendations on how to mitigate the error.

The dreaded and often asked "Error 1009: Cannot access a property or method of a null object reference" is a signal that some of the data appears null, but is tried to be used as a populated object. There are pretty many types of issues that can cause this behavior, and each one should be tested against the code where the error arised.

## Forgotten event listener
    addEventListener(Event.ENTER_FRAME,moveChild);
    function moveChild(e:Event):void {
        childMC.x++;
        if (childMC.x>1000) {
            gotoAndStop(2);
        }
    }

This example will move the `childMC` (added to `Main` at design time) but will instantly throw a 1009 as soon as `gotoAndStop()` is invoked, if that `childMC` does not exist on frame 2. The primary reason for this is that whenever a playhead passes a key frame (a frame which doesn't inherit the previous frame's object set), either by using `gotoAndStop()`, `gotoAndPlay()` with destination frame being separated from the current frame by a keyframe, or by normal play if the SWF is an animation, the current frame's contents are **destroyed** and the new contents are created using the data stored from GUI. So, if the new frame does not have a child named `childMC`, the property request will return null and 1009 will be thrown.

The same principle applies if you add two event listeners, but remove only one, or add a listener to one object, but try removing from another. The `removeEventListener` call won't warn you if the object did not have a respective event listener attached, so read the code that adds and removes event listeners carefully.

Note also: Using `Timer` objects, calling [`setInterval()` and `setTimeout()`][1] also creates event listeners, and these should also be cleared properly.


  [1]: http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/utils/package-detail.html

## Stage is unavailable
Sometimes developers write some code that desires access to `stage`, or Flash stage, to add listeners. It can work for the first time, then all of a sudden fail to work and produce the error 1009. The code in question can even be on the timeline, as it's the first initiative to add code there, and many tutorials that still exist use timeline code layer to place the code.

    public class Main extends MovieClip {
        public function Main() {
            stage.addEventListener(Event.ENTER_FRAME,update); // here

The reason this code doesn't work is simple: A display object is first instantiated, then added to the display list, and while it's off the display list, `stage` is null.

Worse if the code like this:

    stage.addEventListener(Event.ENTER_FRAME,update); // here

is placed on the timeline. It can even work for some time, while the `Main` object is slapped to stage via GUI. Then, their SWF is loaded from another SWF, and all of a sudden the code breaks. This happens because the `Main`'s frames are constructed in a different way when the SWF is loaded directly by the player and when the loading is processed asynchronously. The solution is to use `Event.ADDED_TO_STAGE` listener, and put all code that addresses stage in it, and put the listener itself into an AS file instead of the timeline.

## Invalid typecast
    function listener(e:Event):void {
        var m:MovieClip=e.target as MovieClip;
        m.x++;
    }

If such a listener is attached to an object that's not a `MovieClip` descendant (for example, a `Sprite`), the typecast will fail, and any subsequent operations with its result will throw the 1009 error.

## Uninstantiated object
    var a:Object;
    trace(a); // null
    trace(a.b); // Error 1009

Here, an object reference is declared, but is never assigned a value, be it with `new` or assignment of a non-null value. Requesting its properties or method results in a 1009 error.

## Multi-tiered expression
    x=anObject.aProperty.anotherProperty.getSomething().data;

Here, any object before the dot can end up being null, and using methods that return complex objects only increases the complication to debug the null error. Worst case if the method is prone to extraneous failures, say retrieving data over the network.

## Unprocessed function result
    s=this.getChildByName("garbage");
    if (s.parent==this) {...}

[`getChildByName()`][1] is one of the many functions that can return null if an error occurred when processing its input. Therefore, if you are receiving an object from any function that can possibly return null, check for null first. Here, a property is instantly queried without first checking if `s` is null, this will generate the 1009 error.


  [1]: http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/DisplayObjectContainer.html#getChildByName%28%29

## Invalidated reference to a frame-based object
Sometimes `gotoAndStop()` is called in the middle of the code that refers some frame-based properties. But, **right after the frame is changed** all links to properties that existed on the current frame are invalidated, so any processing that involves them should be immediately terminated.

There are two general scenarios of such processing to occur: First, a loop doesn't end after `gotoAndStop()` call, like here:

    for each (bullet in bullets) {
        if (player.hitTestObject(bullet)) gotoAndStop("gameOver");
    }

Here, a 1009 error means that the `player` MC was destroyed while processing `gotoAndStop()` call, but the loop continues, and refers the now-null link to get `hitTestObject()` from. If the condition would say `if (bullet.hitTestObject(player))` instead, the error would be #2007 "Parameter hitTestObject must not be null". The solution is to put a `return` statement right after calling `gotoAndStop()`.

Second case is multiple event listeners on the same event. Like this:

    stage.addEventListener(Event.ENTER_FRAME,func1);
    stage.addEventListener(Event.ENTER_FRAME,func2);
    function func1(e:Event):void {
        if (condition()) {
            gotoAndStop(2);
        }
    }

Here, if `condition()` is true, the first listener would perform `gotoAndStop()`, but the second listener would still be executed, and if that one references objects on the frame, a 1009 error will be thrown. The solution is to avoid multiple listeners on a single event, in a single object, it's better to have one listener that handles all situations on that event and can properly terminate if a frame change is needed.

