---
title: "Transformation"
slug: "transformation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
- transform="_[functions]*_"
- translate(x[,y])
- rotate(θ[,x,y])
- scale(x[,y])
- skewX(θ)
- skewY(θ)
- matrix(a,b,c,d,e,f)

## Transformation functions
# Translate
`translate` moves graphics along specified vectors:

    <circle cx="0" cy="0" r="50" transform="translate(50,50)"/>
The first value is the x translation, and the second the y. If the y is omitted, it will default to 0.
# Scale
`scale` resizes elements by specified ratios:

    <circle cx="50" cy="50" r="25" transform="scale(.5,2)"/>
Like `translate`, the arguments are x, then y. However, in `scale`, if the y is omitted, it will default to the value of x; in other words, it scales the element without changing the aspect ratio.
# Rotate
`rotate` rotates elements by specified angles.

    <!-- <rect> used for this example because circles can't be rotated -->
    <rect width="100" height="5" transform="rotate(90,50,50)"/>
The first value is the angle, in degrees. The transformation is applied clockwise. The other two values represent the point to be rotated around, defaulting to the origin.

## Applying Transformations
Transformations can be applied to elements by adding a `transform` attribute:

    <circle cx="0" cy="0" r="50" transform="translate(50,50)"/>
Or to groups of elements enclosed in `<g>` tags:

    <g transform="translate(50,50)">
    <circle cx="0" cy="0" r="50"/>
    <circle cx="0" cy="0" r="25" fill="white"/>
    </g>
More transformations can be applied to the same element by adding them separated by spaces:

    <circle cx="0" cy="0" r="50" transform="translate(50,50) scale(.5)"/>

