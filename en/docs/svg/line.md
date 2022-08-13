---
title: "Line"
slug: "line"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Parameters
| Attribute | Description |
| ------ | ------ |
| x1   | Horizontal position of start of line. |
| y1   | Vertical position of start of line.   |
| x2   | Horizontal position of end of line.   |
| y2   | Vertical position of end of line.   |
| stroke   | Color of line. |
| stroke-width   | Width of line.   |
| stroke-opacity| Opacity of line.   |
| stroke-dasharray   | Dash pattern for the line  |
| stroke-linecap| How line ends render  |

Detailed information on the SVG 'line' element can be found in the [W3C Recommendation for SVG][1].


  [1]: https://www.w3.org/TR/SVG/shapes.html#LineElement

## Dashed line drawing with stroke-dasharray
    <svg width="400px" height="400px" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <line x1="10" y1="10" x2="300" y2="10" stroke="red" stroke-width="10" stroke-dasharray="20,2,5,2"/>
    </svg>

Result 

[![enter image description here][1]][1]


## Different examples of stroke-dasharray: 
    
    <svg width="200" height="200" viewBox="0 0 200 200" version="1.1" xmlns="http://www.w3.org/2000/svg"> 
      <line stroke-dasharray="5, 5"              x1="10" y1="10" x2="190" y2="10" />
      <line stroke-dasharray="5, 10"             x1="10" y1="30" x2="190" y2="30" />
        <line stroke-dasharray="10, 5"             x1="10" y1="50" x2="190" y2="50" />
        <line stroke-dasharray="5, 1"              x1="10" y1="70" x2="190" y2="70" />
        <line stroke-dasharray="1, 5"              x1="10" y1="90" x2="190" y2="90" />
        <line stroke-dasharray="0.9"               x1="10" y1="110" x2="190" y2="110" />
        <line stroke-dasharray="15, 10, 5"         x1="10" y1="130" x2="190" y2="130" />
        <line stroke-dasharray="15, 10, 5, 10"     x1="10" y1="150" x2="190" y2="150" />
        <line stroke-dasharray="15, 10, 5, 10, 15" x1="10" y1="170" x2="190" y2="170" />
        <line stroke-dasharray="5, 5, 1, 5"        x1="10" y1="190" x2="190" y2="190" />
        <style><![CDATA[
        line{
            stroke: red;
            stroke-width: 2;
        }
        ]]></style>
     </svg>

Result:

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/6JDKp.png
  [2]: https://i.stack.imgur.com/tS8LF.png

## Line cap alternatives using stroke-linecap
    <svg width="600px" height="400px" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      
        <line x1="10" y1="20" x2="300" y2="20" stroke="red" stroke-width="20" stroke-linecap="butt"/>
          <text x="320" y="20">stroke-linecap="butt" (default)</text>
        <line x1="10" y1="70" x2="300" y2="70" stroke="red" stroke-width="20" stroke-linecap="round"/>
          <text x="320" y="70">stroke-linecap="round"</text>
        <line x1="10" y1="120" x2="300" y2="120" stroke="red" stroke-width="20" stroke-linecap="square"/>
          <text x="320" y="120">stroke-linecap="square"</text>
    </svg>

Result

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/6tHbQ.png

## Draw a cross using diagonal red lines
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <line x1="10" y1="10" x2="100" y2="100" stroke="red" stroke-width="10" />
        <line x1="100" y1="10" x2="10" y2="100" stroke="red" stroke-width="10" />
    </svg>

Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/wxv9L.png

