---
title: "Polyline"
slug: "polyline"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- `<polyline points="10,5 25,15 20,10" />`

## Parameters
Parameter | Details
---|---
points | The points attribute defines a list of points. Each point is defined by a x and a y coordinate in the user coordinate system.|
stroke-width|Width of stroke|
stroke-opacity|Opacity of stroke|
stroke-dasharray|(Optional) Specifies the dash pattern for the stroke|
stroke-linecap |(Optional) Specifies whether line end should be flush, round or squared off ("butt" (default)/"round"/"square")|
stroke-linejoin|(Optional) Specifies how line segments should be joined - mitered, rounded or beveled ("miter" (default)/"round"/"bevel")
stroke-miterlimit|(Optional) Specifies the maximum dimension of a miter. Mitered joins that exceed this limit are converted to a bevel join. Default="4"|


## Polylines with alternative linejoins, linecaps and miterlimits
    <svg width="600px" height="600px" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      
        <polyline points="10,10,50,40,80,30,120,90,130,10,180,50,250,100,300,10" fill="none" stroke="red" stroke-width="10" />
      
          <text x="320" y="20">Default drawing stroke</text>
      
          <g transform="translate(0,150)">
          <polyline points="10,10,50,40,80,30,120,90,130,10,180,50,250,100,300,10" fill="none" stroke="red" stroke-width="10" stroke-linecap="butt" stroke-linejoin="miter" stroke-miterlimit="2"/>
      
          <text x="320" y="20">stroke-linecap="butt" (default)</text>
          <text x="320" y="40">stroke-linejoin="miter"(default)</text>
          <text x="320" y="60">stroke-miterlimit="2"</text>
      </g>
    
      <g transform="translate(0,300)">
          <polyline points="10,10,50,40,80,30,120,90,130,10,180,50,250,100,300,10" fill="none" stroke="red" stroke-width="10" stroke-linecap="round" stroke-linejoin="round" />
      
          <text x="320" y="20">stroke-linecap="round" </text>
          <text x="320" y="40">stroke-linejoin="round" </text>
    
      </g>
      <g transform="translate(0,450)">
          <polyline points="10,10,50,40,80,30,120,90,130,10,180,50,250,100,300,10" fill="none" stroke="red" stroke-width="10" stroke-linecap="square" stroke-linejoin="bevel"/>
      
          <text x="320" y="20">stroke-linecap="square"</text>
          <text x="320" y="40">stroke-linejoin="bevel"</text>
      </g>
      
    </svg>

Result

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/gMDiG.png

## SVG including a polyline
    <svg xmlns="http://www.w3.org/2000/svg" version="1.1">
      <polyline points="10,5 25,15 20,10" />
    </svg>

