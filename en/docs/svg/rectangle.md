---
title: "Rectangle"
slug: "rectangle"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Parameters
| Attribute | Description |
| ------ | ------ |
| x   | Horizontal position of rectangle from left margin.    |
| y   | Vertical position of rectangle from top margin.   |
| width   | Width of rectangle.   |
| height   | Height of rectangle.   |
| rx | Horizontal radius of ellipse used to round corners of rectangle |
| ry | Vertical radius of ellipse used to round corners of rectangle |
| stroke   | Colour of rectangle border.   |
| stroke-width   | Width of rectangle border.   |
| fill   | Colour *inside* rectangle border.  |

Detailed information on the SVG 'rect' element can be found in the [W3C Recommendation for SVG][1].


  [1]: https://www.w3.org/TR/SVG/shapes.html#RectElement

## Draw a black rectangle with yellow fill and rounded corners
 - The `width` and `height` attributes designate the dimensions of the rectangle. These values are in pixels by default
 - The `fill` value sets the color for the rectangle. If no value for `fill` is specified, black is used by default


    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <rect x="10" y="10" width="50" height="100" rx="10" ry="10" stroke="black" stroke-width="5" fill="yellow" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/4qH8I.png

## Draw a black rectangle without fill
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <rect x="10" y="10" width="50" height="100" stroke="black" stroke-width="5" fill="none" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/cLEcD.png

