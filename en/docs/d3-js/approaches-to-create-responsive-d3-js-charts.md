---
title: "Approaches to create responsive d3.js charts"
slug: "approaches-to-create-responsive-d3js-charts"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax
 - var width = document.getElementById('chartArea').clientWidth;
 - var height = width / 3.236;
 - window.onresize = resizeFunctionCall;

## Using bootstrap
One approach that is employed often is to use [bootstrap][1]'s gridded framework in order to define the area that the chart will exist in. Using this in conjunction with [`clientWidth`][2] variable and the [`window.onresize`][3] event, it is very easy to create responsive d3 SVGs.

Let's first create a row and a column that our chart will be built in.

## index.html ##
```
<!DOCTYPE html>
<html>
<head>
<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">
</head>
<body>
  <div class="container">
    <div class="row">
      <div class="col-xs-12 col-lg-6" id="chartArea">
      </div>
    </div>
  </div>
<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/4.1.1/d3.js"></script>
<script src="chart.js"></script>
</body>
</html>
```
This will create a colum that will be full screen on mobile device and half on a large screen.

## chart.js ##
```
var width = document.getElementById('chartArea').clientWidth;
//this allows us to collect the width of the div where the SVG will go.
var height = width / 3.236;
//I like to use the golden rectangle ratio if they work for my charts.

var svg = d3.select('#chartArea').append('svg');
//We add our svg to the div area


//We will build a basic function to handle window resizing.
function resize() {
    width = document.getElementById('chartArea').clientWidth;
    height = width / 3.236;
    d3.select('#chartArea svg')
      .attr('width', width)
      .attr('height', height);
}

window.onresize = resize;
//Call our resize function if the window size is changed.
```

This is an extremely simplified example, but does cover the basic concepts of how to setup charts to be responsive. The resize function will need to make a call to your main update function that will redraw all paths, axis, and shapes just as if the underlying data had been updated. Most d3 users who are concerned with responsive visualizations will already know how to build their update events into functions that are easy to call as shown in [the intro topic][4] and [this topic][5].


  [1]: http://getbootstrap.com/
  [2]: https://developer.mozilla.org/en-US/docs/Web/API/Element/clientWidth
  [3]: https://developer.mozilla.org/en-US/docs/Web/API/GlobalEventHandlers/onresize
  [4]: https://www.wikiod.com/d3-js/getting-started-with-d3js
  [5]: https://www.wikiod.com/d3-js

