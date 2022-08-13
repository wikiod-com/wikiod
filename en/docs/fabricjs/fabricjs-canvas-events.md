---
title: "Fabricjs canvas events"
slug: "fabricjs-canvas-events"
draft: false
images: []
weight: 9873
type: docs
toc: true
---

## Syntax
1. **on(eventName, handler)** - Attaches an event listener with a callback to the object.

2. **off(eventName, handler)** - Remove the event listener from the object. Calling this function witout any arguments will remove all event listeners on the object.

3. **trigger(eventName, optionsopt)** - Fires the event and optional options object.

## Parameters
| Parameter | Description|
| ------ | ------ |
| `eventName`| The name of the event you want to subscribe such as 'object:moving'|
| `eventHandler`| The function you want to execute when that particluar event is triggered|
| `optionsopt`| Options object|

Fabric supports a number of events to allow for interactivity and extensibility. In order to subscribe to events for a canvas use the `on` method the way its used in jQuery. And you wish to manually trigger any event use the `trigger` method. All the events are within the scope of a particular canvas instance. Visit [Link][1] for more information on events


  [1]: https://github.com/kangax/fabric.js/wiki/Working-with-events

## Fabric js canvas events Demo
<!-- language: lang-html -->
    <canvas id="c" width="400" height="400"></canvas>

<!-- language: lang-js -->
    var canvas = new fabric.Canvas("c");

    canvas.on('mouse:up', function () {
      console.log('Event mouse:up Triggered');
    });

    canvas.on('mouse:down', function () {
      console.log('Event mouse:down Triggered');
    });

    canvas.on('after:render', function () {
      console.log('Event after:render Triggered');
    });

    canvas.on('object:moving', function () {
      console.log('Event object:moving Triggered');
    });

    canvas.on('object:modified', function () {
      console.log('Event object:modified Triggered');
    });

    var text = new fabric.Textbox('Hello world', {
      width:250,
      cursorColor :"blue"
    });
    canvas.add(text);

The code above displays how the event API in Fabric.js works. By calling
`on` on the canvas instance, or even on the Fabric.js other objects,
such as `Rect` instance, you can listen to their events and when the 
listeners are triggered, the callback you passed to them will be 
triggered as well.

