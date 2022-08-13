---
title: "Event Handling"
slug: "event-handling"
draft: false
images: []
weight: 9919
type: docs
toc: true
---

 - Here's a [plunker](http://plnkr.co/edit/Jgf8LpmOVUQ1iTZTu9k5) for all the examples
 - Attribute's name are case-insensitive and will always be converted to lowercase, e.g if you have an attribute `on-myListener` listener will be set on `mylistener` event.
 - Similar to `listen` you can also use `unlisten` method remove any listener.
 - Poperty change fires an event by the name of `property-changed` e.g if property is `myProperty` event will be `my-propert-changed`(camel casing to '-'). You can listen to them either by using `on-event` attribute of `listener` Object. 
 - New value is always stored in `e.detail.value` for property change events.

## Event listening using listener Object
    <dom-module id="using-listeners-obj">
      <template>
        <style>
          :host{
            width: 220px;
            height: 100px;
            border: 1px solid black;
            display: block;
          }
    
          #inner{
            width: calc(100% - 10px);
            height: 50px;
            border: 1px solid blue;
            margin: auto;
            margin-top: 15px;
          }
        </style>
        <div>Tap me for alert message</div>
        <div id="inner">Tap me for different alert</div>
      </template>
    </dom-module>
    <script>
      Polymer({
        is:'using-listeners-obj',
    
        //Listener Object
        listeners:{
          //tap a special gesture event in Polymer. Its like click event the main difference being it is more mobile deivce friendly
          'tap':'tapped',  //this will get executed on host of the element
          'inner.tap':'divTapped' //this tap will get executed only on the mentioned node (inner in this case)
        },
    
        tapped:function(){
          alert('you have tapped host element');
        },
    
        divTapped:function(e){
          event.stopPropagation();  //this is only to stop bubbling of event. If you comment this then tap event will bubble and parent's(host) listener will also get executed.
          console.log(e);
          alert('you have tapped inner div');
        }
      })
    </script>

## Annotated Listener 
Another way to add an event listener is to use `on-event` annotation in DOM. Below is an example using `up` event, which is fired when finger/mouse goes up

    <dom-module id="annotated-listener">
      <template>
        <style>
          .inner{
            width: calc(200px);
            height: 50px;
            border: 1px solid blue;
            margin-top: 15px;
          }
        </style>
        <!-- As up is the name of the event annotation will be on-up -->
        <div class="inner" on-up='upEventOccurs'>Tap me for different alert</div>
      </template>
    </dom-module>
    <script>
      Polymer({
        is:'annotated-listener',
        upEventOccurs:function(e){
          //detail Object in event contains x and y co-ordinate of event
          alert('up event occurs at x:'+e.detail.x+' y:'+e.detail.y);
        }
      })
    </script>

## Imperative listener
You can also add/remove listener imperatively using [listen](https://www.polymer-project.org/1.0/docs/api/Polymer.Base#method-listen) and [unlisten](https://www.polymer-project.org/1.0/docs/api/Polymer.Base#method-unlisten) method of Polymer

        <dom-module id="imparative-listener">
          <template>
            <style>
              #inner{
                width: 200px;
                height: 50px;
                border: 1px solid blue;
                margin-top: 15px;
              }
            </style>
            <div id="inner">I've imparative listener attached</div>
          </template>
        </dom-module>
        <script>
          Polymer({
            is:'imparative-listener',
            
            //keeping it in attached to make sure elements with their own shadow root are attached
            attached:function(){
              this.listen(this.$.inner,'track','trackDetails'); // For more details on method check https://www.polymer-project.org/1.0/docs/api/Polymer.Base#method-listen
            },
            
            //all the listener functions have event as first parameter and detail (event.detail) as second parameter to the function
            trackDetails:function(e,detail){
              if(detail.state=='end'){
                alert('Track distance x:'+detail.dx+' y:'+detail.dy);// For more details on track event check https://www.polymer-project.org/1.0/docs/devguide/gesture-events
          }
        }
      })
    </script>

## Custom Events
You can also fire your own events and then listen to them from either Polymer element of HTML page

This Element fires the custom event

        <dom-module id="custom-event">
      <template>
        <style>
          #inner{
            width: 200px;
            height: 50px;
            border: 1px solid blue;
            margin-top: 15px;
          }
        </style>
        <div id="inner" on-tap="firing">I'll fire a custom event</div>
      </template>
    </dom-module>
    <script>
      Polymer({
        is:'custom-event',
        firing:function(){
          this.fire('my-event',{value:"Yeah! i'm being listened"}) //fire is the method which is use to fire custom events, second parameter can also be null if no data is required
        }
      })
    </script>

Here's an element which is listening to that custom event

      <link rel="import" href="custom-event.html">
     <dom-module id="custom-event-listener">
      <template>
        <style></style>
        <custom-event on-my-event="_myListen"></custom-event>
      </template>
    </dom-module>
    <script>
      Polymer({
        is:'custom-event-listener',
    /*    listeners:{ //you can also use listener object instead of on-event attribute
          'my-event':'listen'
        },*/
        _myListen:function(e,detail){
          alert(detail.value+"from Polymer Element");
        },
      })
    </script>

Listening from HTML

    <html>
      <head>
        <meta charset="UTF-8">
        <title>Events</title>
        <script src='bower_components/webcomponentsjs/webcomponents-lite.min.js'></script>
        <link rel="import" href="./bower_components/polymer/polymer.html">
        <link rel="import" href="custom-event-listener.html">
      </head>
      <body>
        <!--As event bubbles by default we can also listen to event from custom-event-listener instead of custom-event-->
        <custom-event-listener></custom-event-listener>
      </body>
      <script>
        document.querySelector('custom-event-listener').addEventListener('my-event',function(e){
          console.log(e);
          alert(e.detail.value+"from HTML");
        })
      </script>
    </html>

## Property change event
Properties with `notify:true` also fires an event

    <link rel="import" href="../bower_components/paper-input/paper-input.html">
    <dom-module id="property-change-event">
      <template>
        <style></style>
        <paper-input id="input" label="type here to fire value change event" on-value-changed='changeFired'></paper-input>
      </template>
    </dom-module>
    <script>
      Polymer({
        is:'property-change-event',
        changeFired:function(e){
          if(e.detail.value!="")
            alert("new value is: "+e.detail.value);
        }
      })
    </script>

## Event Retargeting
It is possible to retarget an event in Polymer ie you can change event details like `path` thus hiding the actual details of an event/element from user. 

For e.g if a `div` inside `event-retargeting` element is firing the event but developer does not want the user to know that he can retarget the event to `event-retargeting` element by using the following code.

    var targetEl = document.querySelector('event-retargeting');
    var normalizedEvent = Polymer.dom(event);
    normalizedEvent.rootTarget = targetEl;
    normalizedEvent.localTarget =targetEl
    normalizedEvent.path = [];
    normalizedEvent.path.push(targetEl);
    normalizedEvent.path.push(document.querySelector('body'));
    normalizedEvent.path.push(document.querySelector('html')); 

To see the working example please refer to `plunker` in `remarks` section.

