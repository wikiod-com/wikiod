---
title: "mask"
slug: "mask"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

The `mask` element allows you to "clip" with soft edges. You can compose masks from multible elements including text. Everything of a mask that is white will be completely opaque. Everything that is black will be completely transparent. Values between white and black will be semi transparent. 

Be aware that masks are a computational expensive operation. The calculation needs to be made for every pixel in the area of the mask. So keep the area of your mask as small as possible.

## basic mask
A green rect with a round hole in the middle showing the image background underneth.
    
    <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <mask id="myMask">
            <rect x="0" y="0" width="100" height="100" fill="white"/>
            <circle cx="50" cy="50" r="45" fill="black"/>
        </mask>
        <image xlink:href="https://cdn.pixabay.com/photo/2013/04/06/05/06/ship-100978_960_720.jpg" width="100" height="100"/>
        <rect x="0" y="0" width="100" height="100" fill="green" mask="url(#myMask)"/>
    </svg>

## complex example with text and shapes
A green rect with a complex mask applied to it showing the background image.

    <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <mask id="myMask0">
            <circle cx="50" cy="50" r="30" fill="white"/>
        </mask>
        <mask id="myMask">
            <rect x="0" y="0" width="100" height="100" fill="white"/>
            <text x="5" y="60" font-size="40">Mask</text>
            <circle cx="50" cy="50" r="30" fill="black"/>
            <text x="5" y="60" font-size="40" mask="url(#myMask0)" fill="white">Mask</text>
        </mask>
        <image xlink:href="https://cdn.pixabay.com/photo/2013/04/06/05/06/ship-100978_960_720.jpg" width="100" height="100"/>
        <rect x="0" y="0" width="100" height="100" fill="green" mask="url(#myMask)"/>
    </svg>

## semi transparency
a green rect (again...) with 4 holes created using 4 greyscale values resulting in 4 different opacities.

    <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <mask id="myMask">
            <rect x="0" y="0" width="100" height="100" fill="white"/>
            <circle cx="25" cy="25" r="20" fill="black"/>
            <circle cx="75" cy="25" r="20" fill="#333"/>
            <circle cx="25" cy="75" r="20" fill="#666"/>
            <circle cx="75" cy="75" r="20" fill="#999"/>
        </mask>
        <image xlink:href="https://cdn.pixabay.com/photo/2013/04/06/05/06/ship-100978_960_720.jpg" width="100" height="100"/>
        <rect x="0" y="0" width="100" height="100" fill="green" mask="url(#myMask)"/>
    </svg>

## a mask with a gradient
A green rect with a hole in the middle, with soft edges.

    <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <radialGradient id="rg">
          <stop offset="0" stop-color="black"/>
          <stop offset="1" stop-color="white"/>
        </radialGradient>
        <mask id="myMask">
            <rect x="0" y="0" width="100" height="100" fill="white"/>
            <circle cx="50" cy="50" r="45" fill="url(#rg)"/>
        </mask>
        <image xlink:href="https://cdn.pixabay.com/photo/2013/04/06/05/06/ship-100978_960_720.jpg" width="100" height="100"/>
        <rect x="0" y="0" width="100" height="100" fill="green" mask="url(#myMask)"/>
    </svg>

