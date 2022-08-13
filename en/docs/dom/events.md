---
title: "Events"
slug: "events"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Parameters
| Parameter       | Description |
| ------------ | ----------- |
| **type** | `String` defines the name of the event to listen to. |
| **listener** | `Function` triggers when the event occurs. |
| **options**       | `Boolean` to set capture, if `Object` you can set the following properties on it,  notice that the object option is weakly supported. |
| 1. *capture*   | A Boolean that indicates that events of this type will be dispatched to the registered listener before being dispatched to any EventTarget beneath it in the DOM tree.    |
| 2. *once*   | A Boolean indicating that the listener should be invoked at most once after being added. If it is true, the listener would be removed automatically when it is invoked.   |
| 3. *passive*   | A Boolean indicating that the listener will never call preventDefault(). If it does, the user agent should ignore it and generate a console warning.   |

**<h1> Origin of events  </h1>**

[![Events dont start at the thing you trigger the event on.][1]][1]

Events dont start at the thing you trigger the event on (a button for example).

<h2> Instead </h2>

It touches every element in its path and it inform every element that an event is happening. Events also go back up after they reach their destination, informing the elements again of its occurrence.

<img src="http://i.stack.imgur.com/8fCuu.png" width="250">


**<h1> Capturing & Bubbling  </h1>**

As we learned, events start from the top of DOM tree, informs every node in its path down to its destination, then goes back up when it reaches its destination, also informing every element it touches on its way up about its occurrence.

> Events going down the DOM tree are in the **capturing phase**, events going up the DOM tree are in the **bubbling phase**.

By default events are listened to in the bubbling phase. To change this you can specify which phase the event gets listened to by specifying the third parameter in the addEventListener function. (code example in the *capture* section)

<img src="http://i.stack.imgur.com/1r40v.png" width="350">





  [1]: http://i.stack.imgur.com/GGaLx.png
  [2]: http://i.stack.imgur.com/1r40v.png

## Event Bubbling and Capturing
Events fired on DOM elements don't just affect the element they're targeting. Any of the target's ancestors in the DOM may also have a chance to react to the event. Consider the following document:

    <!DOCTYPE html>
    <html>
    <head>
    <meta charset="utf-8" />
    </head>
    <body>
      <p id="paragraph">
        <span id="text">Hello World</span>
      </p>
    </body>
    </html>

If we just add listeners to each element without any options, then trigger a click on the span...

    document.body.addEventListener('click', function(event) {
      console.log("Body clicked!");
    });
    window.paragraph.addEventListener('click', function(event) {
      console.log("Paragraph clicked!");
    });
    window.text.addEventListener('click', function(event) {
      console.log("Text clicked!");
    });
    
    window.text.click();

...then the event will **bubble** up through each ancestor, triggering each click handler on the way:

    Text clicked!
    Paragraph clicked!
    Body clicked!

If you want one of your handlers to stop the event from triggering any more handlers, it can call the `event.stopPropagation()` method. For example, if we replace our second event handler with this:

    window.paragraph.addEventListener('click', function(event) {
      console.log("Paragraph clicked, and that's it!");
      event.stopPropagation();
    });

We would see the following output, with `body`'s `click` handler never triggered:

    Text clicked!
    Paragraph clicked, and that's it!

Finally, we have the option to add event listeners that trigger during "**capture**" instead of bubbling. Before an event bubbles up from an element through its ancestors, it's first "captured" down to the element through its ancestors. A capturing listener is added by specifying `true` or `{capture: true}` as the optional third argument to `addEventListener`. If we add the following listeners to our first example above:

    document.body.addEventListener('click', function(event) {
      console.log("Body click captured!");
    }, true);
    window.paragraph.addEventListener('click', function(event) {
      console.log("Paragraph click captured!");
    }, true);
    window.text.addEventListener('click', function(event) {
      console.log("Text click captured!");
    }, true);

We'll get the following output:

    Body click captured!
    Paragraph click captured!
    Text click captured!
    Text clicked!
    Paragraph clicked!
    Body clicked!





By default events are listened to in the bubbling phase. To change this you can specify which phase the event gets listened to by specifying the third parameter in the addEventListener function. (To learn about capturing and bubbling, check *remarks*)

    element.addEventListener(eventName, eventHandler, useCapture)

useCapture: `true` means listen to event when its going down the DOM tree. `false` means listen to the event while its going up the DOM tree.

    window.addEventListener("click", function(){alert('1: on bubble')}, false);
    window.addEventListener("click", function(){alert('2: on capture')}, true);

The alert boxes will pop up in this order:

- 2: on capture
- 1: on bubble

**<h1>Real-world use cases</h1>**

Capture Event will be dispatch before Bubble Event, hence you can ensure than an event is listened to first if you listen to it in its capture phase.

if you are listening to a click event on a parent element, and another on its child, you can listen to the child first or the parent first, depending on how you change the useCapture parameter.

[![in bubbling, child event gets called first, in capture, parent first][1]][1]

> in bubbling, child event gets called first, in capture, parent first

HTML:

    <div id="parent">
       <div id="child"></div>
    </div>

Javascript:

    child.addEventListener('click', function(e) {
       alert('child clicked!');
    });

    parent.addEventListener('click', function(e) {
       alert('parent clicked!');
    }, true);

Setting true to the parent eventListener will trigger the parent listener first.

Combined with e.stopPropagation() you can prevent the event from triggering the child event listener / or the parent. (more about that in the next example)


  [1]: http://i.stack.imgur.com/nabvw.png

## Introduction
Definition:
> In computing, an event is an action or occurrence recognized by
> software that may be handled by the software. Computer events can be
> generated or triggered by the system, by the user or in other ways.
> *[Definition Source][1]*

[![enter image description here][2]][2]

HTML events are "things" that happen to HTML elements. JavaScript can "react" on these events. via `Event Listeners`. Additionally, custom events can be triggered using `dispatchEvent`. But this is only an introduction, so lets get started!

**<h1>Basic Event Listener</h1>**
To listen to events, you call `target.addEventListener(type, listener);`

    function loadImage() {
      console.log('image code here!');
    }
    var myButton = document.querySelector('#my-button');
    myButton.addEventListener('click', loadImage);

This will trigger loadImage every time `my-button` is clicked.

Event listeners can be attached to any node in the DOM tree.
to see a full list of all the events natively triggered in the browser: go here
[MDN link for full event list][3]


  [1]: https://en.wikipedia.org/wiki/Event_(computing)
  [2]: http://i.stack.imgur.com/59MvU.png
  [3]: https://developer.mozilla.org/en-US/docs/Web/Events

## Event Object
To access the event object, include an `event` parameter in the event listener callback function:

    var foo = document.getElementById("foo");
    foo.addEventListener("click", onClick);

    function onClick(event) {
      // the `event` parameter is the event object
      // e.g. `event.type` would be "click" in this case
    };



**<h1>e.stopPropagation();</h1>**
HTML:

    <div id="parent">
       <div id="child"></div>
    </div>

Javascript:

    var parent = document.querySelector('#parent');
    var child = document.querySelector('#child');

    child.addEventListener('click', function(e) {
       e.stopPropagation();
       alert('child clicked!');
    });

    parent.addEventListener('click', function(e) {
       alert('parent clicked!');
    });

since the child stops the event propagation, and the events are listened to during bubbling phase, clicking on the child will only trigger the child. without stopping the propagation both events will be triggered.

<br>

**<h1>e.preventDefault();</h1>**

The `event.preventDefault()` method stops the default action of an element from happening.

For example:

- Prevent a submit button from submitting a form
- Prevent a link from following the URL


    var allAnchorTags = document.querySelector('a');
    
    allAnchorTags.addEventListener('click', function(e){
        e.preventDefault();
        console.log('anchor tags are useless now! *evil laugh*');
    });

**<h1>e.target vs e.currentTarget</h1>**

> e.currentTarget Identifies the current target for the event, as the event traverses
> the DOM. It always refers to the element the event handler has been
> attached to as opposed to event.target which identifies the element on
> which the event occurred.

in other words

`e.target` will return what triggers the event dispatcher to trigger

`e.currentTarget` will return what you assigned your listener to.


HTML:

    <body>
       <button id="my-button"></button>
    </body>

Javascript:

    var body = document.body;
    body.addEventListener( 'click', function(e) {
        console.log('e.target', e.target);
        console.log('e.currentTarget', e.currentTarget);
    });

if you click `my-button`,
- **e.target** will be `my-button`
- **e.currentTarget** will be `body`



## Removing event listeners
The removeEventListener() method removes event handlers that have been attached with the addEventListener() method:

    element.removeEventListener("mousemove", myFunction);

Everything (eventname, function, and options) in the `removeEventListener` must match the one set when adding the event listener to the element.

**<h1>.bind with removeListener</h1>**

using `.bind` on the function when adding an event listener will prevent the function from being removed, to actually remove the eventListener you can write:

    function onEvent() {
       console.log(this.name);
    }

    var bindingOnEvent = onEvent.bind(this);
    
    document.addEventListener('click', bindingOnEvent);
    
    ...
    
    document.removeEventListener('click', bindingOnEvent);


**<h1>listen to an event only once</h1>**
Until `once` option is widely supported, we have to manually remove the even listener once the event is triggered for the first time.

This small helper will help us achieve this:


    Object.prototype.listenOnce = Object.prototype.listenOnce ||
      function listenOnce(eventName, eventHandler, options) {
          var target = this;
          target.addEventListener(eventName, function(e) {
              eventHandler(e);
              target.removeEventListener(eventName, eventHandler, options);
          }, options);
      }

    var target = document.querySelector('#parent');
    target.listenOnce("click", clickFunction, false);

**It is not a best practice to attach functions to the Object prototype, hence you can remove the first line of this code and add a target to it as a first param.*



## Waiting for the document to load


## Event Delegation


## Triggering custom events
The CustomEvent API allows developers to create custom events and trigger them on DOM nodes, passing data along the way. 

    event = new CustomEvent(typeArg, customEventInit);

typeArg - DOMString representing the name of the event.

customEventInit - is optional parameters (that will be passed as `e` in following example).


You can attach `eventListeners` to `document` or *any* HTML element.

Once custom event has been added and bound to element (or document) one might want to manually fire it from javascript.

    document.addEventListener("event-name", function(e) {
      console.log(e.detail); // logs custom object passed from the event.
    });
    
    var event = new CustomEvent("event-name", { "param-name": "param-value" });
    document.dispatchEvent(event);





