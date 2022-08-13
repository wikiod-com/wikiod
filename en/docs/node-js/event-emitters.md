---
title: "Event Emitters"
slug: "event-emitters"
draft: false
images: []
weight: 9755
type: docs
toc: true
---

When an event "fires" (which means the same as "publishing an event" or "emitting an event"), each listener will be called synchronously ([source][1]), along with any accompanying data that was passed in to `emit()`, no matter how many arguments you pass in:
```js
myDog.on('bark', (howLoud, howLong, howIntense) => {
  // handle the event
})
myDog.emit('bark', 'loudly', '5 seconds long', 'fiercely')
```


The listeners will be called in the order they were registered:
```js
myDog.on('urinate', () => console.log('My first thought was "Oh-no"'))
myDog.on('urinate', () => console.log('My second thought was "Not my lawn :)"'))
myDog.emit('urinate')
// The console.logs will happen in the right order because they were registered in that order.
```

But if you need a listener to fire first, before all of the other listeners that have already been added, you can use `prependListener()` like so:
```js
myDog.prependListener('urinate', () => console.log('This happens before my first and second thoughts, even though it was registered after them'))
```

If you need to listen to an event, but you only want to hear about it once, you can use `once` instead of `on`, or `prependOnceListener` instead of `prependListener`. After the event is fired and the listener gets called, the listener will automatically be removed, and won't be called again the next time the event is fired.

Finally, if you want to remove all of the listeners and start over, feel free to do just that:
```js
myDog.removeAllListeners()
```

  [1]: https://nodejs.org/dist/latest-v6.x/docs/api/events.html#events_asynchronous_vs_synchronous

## Basics
Event Emitters are built into Node, and are for pub-sub, a pattern where a *publisher* will emit events, which *subscribers* can listen and react to. In Node jargon, publishers are called *Event Emitters*, and they emit events, while subscribers are called *listeners*, and they react to the events.

```js
// Require events to start using them
const EventEmitter = require('events').EventEmitter;
// Dogs have events to publish, or emit
class Dog extends EventEmitter {};
class Food {};

let myDog = new Dog();

// When myDog is chewing, run the following function
myDog.on('chew', (item) => {
  if (item instanceof Food) {
    console.log('Good dog');
  } else {
    console.log(`Time to buy another ${item}`);
  }
});

myDog.emit('chew', 'shoe'); // Will result in console.log('Time to buy another shoe')
const bacon = new Food();
myDog.emit('chew', bacon); // Will result in console.log('Good dog')
```

In the above example, the dog is the publisher/EventEmitter, while the function that checks the item was the subscriber/listener. You can make more listeners too:
```js
myDog.on('bark', () => {
  console.log('WHO\'S AT THE DOOR?');
  // Panic
});
```

There can also be multiple listeners for a single event, and even remove listeners:
```js
myDog.on('chew', takeADeepBreathe);
myDog.on('chew', calmDown);
// Undo the previous line with the next one:
myDog.removeListener('chew', calmDown);
```

If you want to listen to a event only once, you can use:

```js
myDog.once('chew', pet);
```

Which will remove the listener automatically without race conditions.

## HTTP Analytics through an Event Emitter
In the HTTP server code (e.g. `server.js`):

<!-- language: lang-js -->

    const EventEmitter = require('events')
    const serverEvents = new EventEmitter()
    
    // Set up an HTTP server
    const http = require('http')
    const httpServer = http.createServer((request, response) => {
      // Handler the request...
      // Then emit an event about what happened
      serverEvents.emit('request', request.method, request.url)
    });
    
    // Expose the event emitter
    module.exports = serverEvents


In supervisor code (e.g. `supervisor.js`):

<!-- language: lang-js -->

    const server = require('./server.js')
    // Since the server exported an event emitter, we can listen to it for changes:
    server.on('request', (method, url) => {
      console.log(`Got a request: ${method} ${url}`)
    })


Whenever the server gets a request, it will emit an event called `request` which the supervisor is listening for, and then the supervisor can react to the event.

## Get the names of the events that are subscribed to
The function **EventEmitter.eventNames()** will return an array containing the names of the events currently subscribed to.

    const EventEmitter = require("events");
    class MyEmitter extends EventEmitter{}

    var emitter = new MyEmitter();

    emitter
    .on("message", function(){ //listen for message event
        console.log("a message was emitted!");
    })
    .on("message", function(){ //listen for message event
        console.log("this is not the right message");
    })
    .on("data", function(){ //listen for data event
        console.log("a data just occured!!");
    });

    console.log(emitter.eventNames()); //=> ["message","data"]
    emitter.removeAllListeners("data");//=> removeAllListeners to data event
    console.log(emitter.eventNames()); //=> ["message"]

[Run in RunKit][1]


  [1]: https://runkit.com/594bb4eaaac7e6001294132c/594bb635aac7e600129413e7

## Get the number of listeners registered to listen for a specific event
The function Emitter.listenerCount(eventName) will return the number of listeners that are currently listening for the event provided as argument

    const EventEmitter = require("events");
    class MyEmitter extends EventEmitter{}
    var emitter = new MyEmitter();

    emitter
    .on("data", ()=>{ // add listener for data event
        console.log("data event emitter");
    });

    console.log(emitter.listenerCount("data"))    // => 1
    console.log(emitter.listenerCount("message")) // => 0
    
    emitter.on("message", function mListener(){ //add listener for message event
        console.log("message event emitted");
    });
    console.log(emitter.listenerCount("data"))    // => 1
    console.log(emitter.listenerCount("message")) // => 1

    emitter.once("data", (stuff)=>{ //add another listener for data event
        console.log(`Tell me my ${stuff}`);
    })

    console.log(emitter.listenerCount("data"))   // => 2
    console.log(emitter.listenerCount("message"))// => 1




