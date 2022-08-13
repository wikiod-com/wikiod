---
title: "Getting started with d3.js"
slug: "getting-started-with-d3js"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
There are a variety of ways to download and use D3.

### Direct Script Download ###

 1. Download and extract [d3.zip][1]
 2. Copy the resulting folder to where you will keep your project's dependencies
 3. Reference d3.js (for development) or d3.min.js (for production) in your HTML:
    `<script type="text/javascript" src="scripts/d3/d3.js"></script>`

### NPM ###

 1. Initialize NPM in your project if you have not done so already: `npm init`
 2. NPM install D3: `npm install --save d3`
 3. Reference d3.js (for development) or d3.min.js (for production) in your HTML:
    `<script type="text/javascript" src="node_modules/d3/build/d3.js"></script>`

### CDN ###

 1. Reference d3.js (for development) or d3.min.js (for production) in your HTML: `<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/d3/4.1.1/d3.js"></script>`

### GITHUB ###
 1. Get any version of d3.js (for development) or d3.min.js (for production) from Github :
`<script type="text/javascript" src="https://raw.githubusercontent.com/d3/d3/v3.5.16/d3.js"></script>`


To link directly to the latest release, copy this snippet:

    <script src="https://d3js.org/d3.v4.min.js"></script>


  [1]: https://d3js.org/

## What is D3? Data-Driven Documents.
We are so used to the name **D3.js** that it's possible to forget that D3 is actually **DDD** (**D**ata-**D**riven **D**ocuments). And that's what D3 does well, a data-driven approach to DOM (Document Object Model) manipulation: D3 binds data to DOM elements and manipulates those elements based on the bounded data.

Let's see a very basic feature of D3 in this example. Here, we won't append any SVG element. Instead, we'll use an SVG already present on the page, something like this:

    <svg width="400" height="400">
        <circle cx="50" cy="50" r="10"></circle>
        <circle cx="150" cy="50" r="10"></circle>
        <circle cx="210" cy="320" r="10"></circle>
        <circle cx="210" cy="30" r="10"></circle>
        <circle cx="180" cy="200" r="10"></circle>
    </svg>

This is a pretty basic SVG, with 5 circles. Right now, those circles are not "bound" to any data. Let's check this last allegation:

In our code, we write:

    var svg = d3.select("svg");
    var circles = svg.selectAll("circle");
    console.log(circles.nodes());

Here, `d3.select("svg")` returns a d3 object containing `<svg width="400" height="400"></svg>` tag and all child tags, the `<circle>`s. Note that if multiple `svg` tags exist on the page, only the first is selected. If you do not want this, you can also select by tag id, like `d3.select("#my-svg")`. The d3 object has built in properties and methods that we will use a lot later.

`svg.selectAll("circle")` creates an object from all `<circle></circle>` elements from within the `<svg>` tag. It can search through multiple layers, so it does not matter if the tags are direct children.

`circles.nodes()` returns the circle tags with all of their properties.

If we look at the console and choose the first circle, we're gonna see something like this:

[![enter image description here][1]][1]

First, we have `attributes`, then `childNodes`, then `children`, and so on... but no data.

**Let's bind some data**

But, what happens if we bind *data* to these DOM elements?

In our code, there is a function that creates an object with two properties, `x` and `y`, with numeric values (this object is inside an array, check the fiddle below). If we bind this data to the circles...

    circles.data(data);

This is what we are going to see if we inspect the console:

[![enter image description here][2]][2]

We have something new just before `attributes`! Something named `__data__`... and look: the values of `x` and `y` are there!

We can, for instance, change the position of the circles based on these data. Have a look [at this fiddle][3].

This is what D3 does best: binding data to DOM elements and manipulating those DOM elements based on the bounded data.


  [1]: http://i.stack.imgur.com/RHt5m.jpg
  [2]: http://i.stack.imgur.com/jnHL4.jpg
  [3]: https://jsfiddle.net/m37b4ekL/2/

## Simple Bar Chart
## index.html ##

    <!doctype html>
    <html>
      <head>
        <title>D3 Sample</title>
      </head>
      <body>
        <!-- This will serve as a container for our chart. This does not have to be a div, and can in fact, just be the body if you want. -->
        <div id="my-chart"></div>
    
        <!-- Include d3.js from a CDN. -->
        <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/d3/4.1.1/d3.js"></script>
    
        <!-- Include our script that will make our bar chart. -->
        <script type="text/javascript" src="chart.js"></script>
      </body>
    </html>

## chart.js ##

    // Sample dataset. In a real application, you will probably get this data from another source such as AJAX.
    var dataset = [5, 10, 15, 20, 25]
    
    // Sizing variables for our chart. These are saved as variables as they will be used in calculations.
    var chartWidth = 300
    var chartHeight = 100
    var padding = 5
    
    // We want our our bars to take up the full height of the chart, so, we will apply a scaling factor to the height of every bar.
    var heightScalingFactor = chartHeight / getMax(dataset)
    
    // Here we are creating the SVG that will be our chart.
    var svg = d3
      .select('#my-chart')           // I'm starting off by selecting the container.
        .append('svg')               // Appending an SVG element to that container.
        .attr('width', chartWidth)   // Setting the width of the SVG.
        .attr('height', chartHeight) // And setting the height of the SVG.
    
    // The next step is to create the rectangles that will make up the bars in our bar chart.
    svg
      .selectAll('rect')                                          // I'm selecting all of the rectangles in the SVG (note that at this point, there actually aren't any, but we'll be creating them in a couple of steps).
      .data(dataset)                                              // Then I'm mapping the dataset to those rectangles.
      .enter()                                                    // This step is important in that it allows us to dynamically create the rectangle elements that we selected previously.
        .append('rect')                                           // For each element in the dataset, append a new rectangle.
          .attr('x', function (value, index) {                    // Set the X position of the rectangle by taking the index of the current item we are creating, multiplying it by the calculated width of each bar, and adding a padding value so we can see some space between bars.
              return (index * (chartWidth / dataset.length)) + padding
            })
          .attr('y', function (value, index) {                    // Set the rectangle by subtracting the scaled height from the height of the chart (this has to be done becuase SVG coordinates start with 0,0 at their top left corner).
            return chartHeight - (value * heightScalingFactor)
          })
          .attr('width', (chartWidth / dataset.length) - padding) // The width is dynamically calculated to have an even distribution of bars that take up the entire width of the chart.
          .attr('height', function (value, index) {               // The height is simply the value of the item in the dataset multiplied by the height scaling factor.
            return value * heightScalingFactor
          })
          .attr('fill', 'pink')                                   // Sets the color of the bars.
    
    /**
     *  Gets the maximum value in a collection of numbers.
     */
    function getMax(collection) {
      var max = 0
    
      collection.forEach(function (element) {
        max = element > max ? element : max
      })
    
      return max
    }

Sample code available at https://github.com/dcsinnovationlabs/D3-Bar-Chart-Example

Demo available at https://dcsinnovationlabs.github.io/D3-Bar-Chart-Example/

## Hello, world!
Create an `.html` file containing this snippet:

    <!DOCTYPE html>
    <meta charset="utf-8">
    <body>
    <script src="//d3js.org/d3.v4.min.js"></script>
    <script>
    
    d3.select("body").append("span")
        .text("Hello, world!");
    
    </script>

See this snippet in action at [this JSFiddle][1].

  [1]: https://jsfiddle.net/r1dd5pho/


## Simple D3 Chart: Hello World!
Paste this code into an empty HTML file and run it in your browser.

    <!DOCTYPE html>

    <body>

    <script src="https://d3js.org/d3.v4.js"></script>    <!-- This downloads d3 library -->

    <script>
    //This code will visualize a data set as a simple scatter chart using d3. I omit axes for simplicity.
    var data = [        //This is the data we want to visualize. 
                        //In reality it usually comes from a file or database.
      {x: 10,    y: 10},
      {x: 10,    y: 20},
      {x: 10,    y: 30},
      {x: 10,    y: 40},
      {x: 10,    y: 50},
      {x: 10,    y: 80},
      {x: 10,    y: 90},
      {x: 10,    y: 100},
      {x: 10,    y: 110},
      {x: 20,    y: 30},
      {x: 20,    y: 120},
      {x: 30,    y: 10},
      {x: 30,    y: 20},
      {x: 30,    y: 30},
      {x: 30,    y: 40},
      {x: 30,    y: 50},
      {x: 30,    y: 80},
      {x: 30,    y: 90},
      {x: 30,    y: 100},
      {x: 30,    y: 110},
      {x: 40,    y: 120},
      {x: 50,    y: 10},
      {x: 50,    y: 20},
      {x: 50,    y: 30},
      {x: 50,    y: 40},
      {x: 50,    y: 50},
      {x: 50,    y: 80},
      {x: 50,    y: 90},
      {x: 50,    y: 100},
      {x: 50,    y: 110},
      {x: 60,    y: 10},
      {x: 60,    y: 30},
      {x: 60,    y: 50},
      {x: 70,    y: 10},
      {x: 70,    y: 30},
      {x: 70,    y: 50},
      {x: 70,    y: 90},
      {x: 70,    y: 100},
      {x: 70,    y: 110},
      {x: 80,    y: 80},
      {x: 80,    y: 120},
      {x: 90,    y: 10},
      {x: 90,    y: 20},
      {x: 90,    y: 30},
      {x: 90,    y: 40},
      {x: 90,    y: 50},
      {x: 90,    y: 80},
      {x: 90,    y: 120},
      {x: 100,    y: 50},
      {x: 100,    y: 90},
      {x: 100,    y: 100},
      {x: 100,    y: 110},
      {x: 110,    y: 50},
      {x: 120,    y: 80},
      {x: 120,    y: 90},
      {x: 120,    y: 100},
      {x: 120,    y: 110},
      {x: 120,    y: 120},
      {x: 130,    y: 10},
      {x: 130,    y: 20},
      {x: 130,    y: 30},
      {x: 130,    y: 40},
      {x: 130,    y: 50},
      {x: 130,    y: 80},
      {x: 130,    y: 100},
      {x: 140,    y: 50},
      {x: 140,    y: 80},
      {x: 140,    y: 100},
      {x: 140,    y: 110},  
      {x: 150,    y: 50},
      {x: 150,    y: 90},
      {x: 150,    y: 120},
      {x: 170,    y: 20},
      {x: 170,    y: 30},
      {x: 170,    y: 40},
      {x: 170,    y: 80},
      {x: 170,    y: 90},  
      {x: 170,    y: 100},
      {x: 170,    y: 110},
      {x: 170,    y: 120},
      {x: 180,    y: 10},
      {x: 180,    y: 50},
      {x: 180,    y: 120},
      {x: 190,    y: 10},
      {x: 190,    y: 50},
      {x: 190,    y: 120},
      {x: 200,    y: 20},
      {x: 200,    y: 30},  
      {x: 200,    y: 40},
      {x: 210,    y: 80},
      {x: 210,    y: 90},
      {x: 210,    y: 100},
      {x: 210,    y: 110},  
      {x: 210,    y: 120},
      {x: 220,    y: 80},  
      {x: 220,    y: 120},
      {x: 230,    y: 80},  
      {x: 230,    y: 120},
      {x: 240,    y: 90},
      {x: 240,    y: 100},  
      {x: 240,    y: 110},
      {x: 270,    y: 70},
      {x: 270,    y: 80},
      {x: 270,    y: 90},
      {x: 270,    y: 100},  
      {x: 270,    y: 120}
    ];

    //The following code chains a bunch of methods. Method chaining is what makes d3 very simple and concise.
    d3.select("body").append("svg").selectAll()  //'d3' calls the d3 library
                                                 //'.select' selects the object (in this case the body of HTML)
                                                 //'.append' adds SVG element to the body
                                                 //'.selectAll()' selects all SVG elements
        .data(data)                              //'.data' gets the data from the variable 'data'
      .enter().append("circle")                  //'.enter' enters the data into the SVG 
                                                 //the data enter as circles with '.append("circle")'
        .attr("r", 3)                            //'.attr' adds/alters atributes of SVG, 
                                                 //such as radius ("r"), making it 3 pixels
        .attr("cx", function(d) { return d.x; }) //coordinates "cx" (circles' x coordinates)
        .attr("cy", function(d) { return d.y; }) //coordinates "cy" (circles' y coordinates)
        .style("fill", "darkblue");              //'.style' changes CSS of the SVG
                                                 //in this case, fills circles with "darkblue" color

    </script>

Here is a [JSFiddle](https://jsfiddle.net/arthurtarasov/958gmmLf/) of the chart.

You can also download the already created HTML file from [GitHub](https://github.com/arthurtarasov/simple-d3-chart-hello-world).

The next step in learning d3 can be following Mike Bostock's (the d3's creator's) tutorial to create a [bar chart from scratch](https://bost.ocks.org/mike/bar/).

