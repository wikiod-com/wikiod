---
title: "Circle"
slug: "circle"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Parameters
| Parameters | Details |
| --- | --- |
| cx | x-coordinate of center of circle. |
| cy | y-coordinate of center of circle. |
| r | Radius of circle. |
| stroke | Colour of circle border. |
| fill | Colour *inside* circle border. |

Detailed information on the SVG 'circle' element can be found in the [W3C Recommendation for SVG][1].


  [1]: https://www.w3.org/TR/SVG/shapes.html#CircleElement

## Draw a black circle without fill
 - The `cx` and `cy` values designate the location of the center of the circle.
 - The `r` attribute specifies the size of the radius of the circle.


    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <circle cx="40" cy="40" r="30" stroke="black" fill="none" />
    </svg>


Result:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/dSo8R.png

