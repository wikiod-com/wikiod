---
title: "On Events"
slug: "on-events"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - .on('mouseover', function)
 - .on('mouseout', function)
 - .on('click', function)
 - .on('mouseenter', function)
 - .on('mouseleave', function)

For a more in depth example where custom events are defined refer [here][1].


  [1]: https://bl.ocks.org/mbostock/5872848

## Attaching basic events on selections
Often times you will want to have events for your objects.

```
function spanOver(d,i){
  var span = d3.select(this);
  span.classed("spanOver",true);
}

function spanOut(d,i){
  var span = d3.select(this);
  span.classed("spanOver", false);
}

var div = d3.select('#divID');

div.selectAll('span')
  .on('mouseover' spanOver)
  .on('mouseout' spanOut)
```
This example will add the class `spanOver` when hovering over a span inside the div with the id `divID` and remove it when the mouse exits the span.

By default d3 will pass the datum of the current span and the index. It's really handy that `this`'s context is the current object as well so that we can do operations on it, like add or remove classes.

You can also just use an anonymous function on the event as well.

```
div.selectAll('span')
  .on('click', function(d,i){ console.log(d); });
```

Data elements can also be added to the current selected object as well.
```
div.selectAll('path')
  .on('click', clickPath);

function clickPath(d,i) {
  if(!d.active) {
    d.active = true;
    d3.select(this).classed("active", true);
  }
  else {
    d.active = false;
    d3.select(this).classed("active", false);
  }
}
```
In this example active is not defined on the selection before the click event is fired. If you were to go back over the path selection though all clicked objects would contain the `active` key.

## Remove event listener
d3.js doesn't have a [`.off()`](http://api.jquery.com/off/) method to detatch existent event listeners. In order to remove an event handler, you have to set it to `null`: 

    d3.select('span').on('click', null)



