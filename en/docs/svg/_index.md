---
title : SVG Tutorial
slug : svg-tutorial
weight : 9918
draft : false
images : []
type : docs
---

Scalable Vector Graphics (SVG) is a [W3C standard](https://www.w3.org/TR/SVG/) for drawing vector images.

Here is a simple standalone SVG file:

    <svg xmlns="http://www.w3.org/2000/svg">
        <circle cx="50" cy="50" r="25" fill="blue"/>
    </svg>

SVG can also be embedded in HTML, in which case the `xmlns` attribute is not required.

Other graphical elements are:
- `<line>`
- `<ellipse>`
- `<path>`
- `<polygon>` and `<polyline>`
- `<text>` including child elements such as `<tspan>` and `<textPath>`

CSS is used for styling although not all CSS properties apply to SVG and SVG itself defines some specific properties such as `fill` and `stroke` that are not used elsewhere.

Shapes can be filled with gradients or patterns and additional raster effects can be achieved using filters.

Clipping is available by using the above graphical elements as clip paths.

Regarding versions of the W3C SVG standard:
 - The current version is [SVG 1.1 (Second Edition)](https://www.w3.org/TR/SVG11/) 
 - The W3C are currently working on a draft of [SVG 2](https://www.w3.org/TR/SVG2/)

