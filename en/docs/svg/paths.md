---
title: "Paths"
slug: "paths"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Paths are the most flexible element of SVG. A path is a series of cubic or quadratic Bezier curves, arranged in connected splines. A path may be open or closed into a loop, or it may be complex with several sub-components. If a path is not simple, the fill rule is important in deciding which areas are inside or outside of the path.

Paths will normally be generated by automatic editors. Typically quadratic paths are used for fonts, and cubic paths for illustrations.


## Parameters
| Attributes / parameters | Description                                            |
| ----------------------- | ------------------------------------------------------ |
| d   | Defines a sequence of drawing commands that create the shape. e.g. d="M 50,60 L50,60". Uppercase drawing commands designate absolute coordinates. Lowercase drawing commands designate relative coordinates. |
| *(...)*|**Drawing Commands**|
|  m/M| Move current drawing position to XY d="M XY"|
|  l/L |Draw a line to X,Y d="L XY"|
|  v/V |Draw a vertical line to Y d="V Y"|
|  h/H |Draw a horizontal line to X d="H X"|
|  a/A |Draw an arc to X,Y with an implied radius of Rx and Ry and a rotation specified by X-axis-rotation. The large arc and sweep flags select which of the 4 possible arcs that satisfy these constraints should be drawn. d="A Rx Ry X-axis-rotation(degrees) large-arc-flag (0/1) sweep-flag(0/1) X,Y". |
|  q/Q |Draw quadratic bezier curve to X,Y using control point X1Y1 d="X1Y1 X Y"|
|  t/T |Draw a shorthand quadratic bezier curve (the control point is calculated as a reflection of the control point of the previous q/Q drawing command through the current drawing position)|
|  c/C |Draw a cubic bezier curve to X,Y using control points X1,Y1 and X2,Y2 d="C X1Y1, X2Y2, XY"|
|  s/S |Draw a shorthand cubic bezier curve (first control point is calculated as a reflection of the second control point of the previous c/C drawing command through the current drawing position).|
| - z/Z |Close the path by drawing a line to start of path (or pathsegment if another z has been used previously)|
| *(...)*|*(end of list)*|
| pathLength | (Optional) Allows the author to specify a nominal pathLength which will be used for calibration in other calculations, for example for text along a path |
| **Stroke Parameters** | *common among to all shape and drawing elements*|
| stroke | Color of path |
| stroke-width | Width of path |

Detailed information on the SVG `path` element can be found in the [W3C Recommendation for SVG][1].


  [1]: https://www.w3.org/TR/SVG/paths.html#PathElement

## Draw a diagonal blue line using the L path command
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <path d="M 10,10 L 100,50" stroke="blue" stroke-width="5" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/xWrZW.png

## Draw a horizontal orange line using the H drawing command
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <path d="M 10,10 H 200" stroke="orange" stroke-width="5" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/x9zB8.png

## Draw a red cross using l (relative line) path commands
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <path d="M 10,10 l 90,90 M 100,10 l -90,90" stroke="red" stroke-width="10" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/wxv9L.png

## Draw a vertical green line using the V path command
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <path d="M 10,10 V 200" stroke="green" stroke-width="5" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/nmoQ2.png

