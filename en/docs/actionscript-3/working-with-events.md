---
title: "Working with Events"
slug: "working-with-events"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Events are pieces of data that a program can create, exchange and react upon. The asynchronous event flow is dispatched over display list by Flash engine as a reaction on external events, such as mouse movements or another frame being displayed. Every other event flow and all event processing is synchronous, so if a piece of code has generated an event, all reactions on it are processed before the next line of code is executed, also if there are several listeners of an event, all of them would have run before the next event could be processed.

There are several major events associated with Flash programming. `Event.ENTER_FRAME` is generated before Flash draws another frame, it signals the entire display list to prepare to be drawn, and can be used as a synchronous timer. `MouseEvent.CLICK` and its siblings can be used to receive mouse input from the user, and `TouchEvent.TOUCH_TAP` is an analogue for touch screens. `KeyboardEvent.KEY_DOWN` and `KEY_UP` provide means to receive user input from the keyboard, however, their usage in mobile department is almost impossible due to devices having no physical keyboard. Finally, `Event.ADDED_TO_STAGE` is dispatched once a display object receives access to stage, and is included in the global display list that receives the entirety of events that can bubble up and down the display list.

Most events in Flash are component specific. If you are designing your own component that will use Flash events, use a `flash.events.Event` descendant class and its static `String` properties to create your component's event set.

## Custom events with event data
    package
    {
        import flash.events.Event;
    
        public class CustomEvent extends Event
        {
            public static const START:String = "START";
            public static const STOP:String  = "STOP";
    
            public var data:*;
    
            public function CustomEvent(type:String, data:*,
                                        bubbles:Boolean=false, cancelable:Boolean=false)
            {
                super(type, bubbles, cancelable);
    
                if (data)
                    this.data = data;
            }
        }
    }



To dispatch a custom event:

    var dataObject:Object = {name: "Example Data"};
    
    dispatchEvent(new CustomEvent(CustomEvent.START, dataObject))

To listen for custom events:

    addEventListener(CustomEvent.STOP, stopHandler);

    function stopHandler(event:CustomEvent):void
    {
        var dataObject:* = event.data;
    }

## Event handling off the display list
    package {
    import flash.events.EventDispatcher;

    public class AbstractDispatcher extends EventDispatcher {

        public function AbstractDispatcher(target:IEventDispatcher = null) {
            super(target);
        }

    }
    }

To dispatch an event on an instance:

    var dispatcher:AbstractDispatcher = new AbstractDispatcher();
    dispatcher.dispatchEvent(new Event(Event.CHANGE));

To listen for events on an instance:

    var dispatcher:AbstractDispatcher = new AbstractDispatcher();
    dispatcher.addEventListener(Event.CHANGE, changeHandler);

    function changeHandler(event:Event):void
    {
    }

## Add your own events


## Simple Mouse Event Structure
Through the use of `event types` you can easily reduce code bloat that often occurs when defining events for many objects on stage by filtering events in 1 function rather than defining many event handling functions.

Imagine we have 10 objects on stage named `object1`, `object2` ... `object10`

You could do the following:

 

    var i: int = 1;
    while(getChildByName("object"+i) != null){
        var obj = getChildByName("object"+i)
        obj.addEventListener(MouseEvent.CLICK, ObjectMouseEventHandler);
        obj.addEventListener(MouseEvent.MOUSE_OVER, ObjectMouseEventHandler);
        obj.addEventListener(MouseEvent.MOUSE_OUT, ObjectMouseEventHandler);
        obj.alpha = 0.75;
        i++;
    }
    
    function ObjectMouseEventHandler(evt:Event)
    {
        if(evt.type == "click")
        {
            trace(evt.currentTarget + " has been clicked");
        }
        else
        {
            evt.currentTarget.alpha = evt.type == "mouseOver" ? 1 : 0.75;
        }
    }

**Benefits to this method include:**
1. Not needing to specify the quantity of objects to apply events to.
2. Not needing to know specifically what object was interacted with yet still apply functionality.
3. Easily applying events in bulk.


## Basic event handling
Flash dispatches [**`Events`**](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/events/Event.html) for most of its objects. One of the most basic event is [**`ENTER_FRAME`**](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/events/Event.html#ENTER_FRAME), which is dispatched (at the framerate of the SWF) on every display list object.

    import flash.display.Sprite;
    import flash.events.Event;

    var s:Sprite = new Sprite();
    s.addEventListener(Event.ENTER_FRAME, onEnterFrame);

    function onEnterFrame(e:Event)
    {
        trace("I am called on every frame !");
    }

This function will be called asynchronously on every frame. This means the function that you assign as the `onEnterFrame` event handler is processed before any other ActionScript code that is attached to the affected frames.


