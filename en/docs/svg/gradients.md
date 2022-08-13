---
title: "Gradients"
slug: "gradients"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Parameters
| common | definition |
| ------ | ------ |
| gradientUnits   | the coordinate system of the gradient attributes. Either objectBoundingBox or userSpaceOnUse  |
| gradientTransform | the transform to apply to the gradient contents |
| spreadMethod | defines what happens outside the gradient boundaries. Either pad, reflect or repeat |
| xlink:href | link to another gradient which provides attributes or content |
| ------ | ------ |
| Linear Gradient | Definition |
| ------ | ------ |
| x1 | defines the gradient vector |
| x2 | see x1 |
| y1 | see x1 |
| y2 | see x1 |
| ------ | ------ |
| Radial Gradient | Definition |
| ------ | ------ |
| cx | the x coordinate of the outer gradient centre |
| cy | the y coordinate of the outer gradient centre |
| r | the outer radius of the gradient. The location of a 100% stop |
| fx | the x coordinate of the inner gradient centre. The location of a 0% stop |
| fy | the y location of the inner gradient centre. The location of a 0% stop |



SVG is case sensitive so remember to use a capital G in the middle.

## linearGradient
    <svg>
      <defs>
            <linearGradient id='g' y1="100%" x2="100%">
                <stop offset='0%' stop-color='yellow' />
                <stop offset='100%' stop-color='green' />
            </linearGradient>
        </defs>
        <rect width='100%' height='100%' fill='url(#g)'/>
    </svg>


## radialGradient
    <svg>
      <defs>
        <radialGradient id="g">
          <stop offset="10%" stop-color="green" />
          <stop offset="90%" stop-color="white" />
        </radialGradient>
      </defs>
    
      <rect width='100%' height='100%' fill='url(#g)'/>
    </svg>

