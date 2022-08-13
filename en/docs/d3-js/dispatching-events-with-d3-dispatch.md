---
title: "Dispatching Events with d3.dispatch"
slug: "dispatching-events-with-d3dispatch"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Syntax
- d3.**dispatch** - create a custom event dispatcher.
- dispatch.**on** - register or unregister an event listener.
- dispatch.**copy** - create a copy of a dispatcher.
- dispatch.**call** - dispatch an event to registered listeners.
- dispatch.**apply** - dispatch an event to registered listeners.

Dispatching is a convenient mechanism for separating concerns with loosely-coupled code: register named callbacks and then call them with arbitrary arguments. A variety of D3 components, such as d3-request, use this mechanism to emit events to listeners. Think of this like Node’s EventEmitter, except every listener has a well-defined name so it’s easy to remove or replace them.

Related Readings
 - [Dispatching Events by Mike Bostock](https://bl.ocks.org/mbostock/5872848)
 - [d3.dispatch Documentation](https://github.com/d3/d3/blob/master/API.md#dispatches-d3-dispatch)
 - [Dispatch Event in NPM][1]


  [1]: https://www.npmjs.com/package/d3-dispatch

## simple usage
    var dispatch = d3.dispatch("statechange");

    dispatch.on('statechange', function(e){ console.log(e) })

    setTimeout(function(){dispatch.statechange('Hello, world!')}, 3000)

