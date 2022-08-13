---
title: "Patterns"
slug: "patterns"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Parameters
| parameter | description |
| --------- | ------------|
| patternUnits | the coordinate system of the pattern attributes either objectBoundingBox (default) or userSpaceOnUse |
| patternContentUnits | the coordinate system of the pattern contents either objectBoundingBox or userSpaceOnUse(default) |
| patternTransform | the transform to apply to the pattern contents |
| x | the x offset of the pattern (default is zero) |
| y | the y offset of the pattern (default is zero)|
| width | the width of the pattern (required)|
| height | the height of the pattern (required)|
| xlink:href | link to another pattern that provides some attributes or content
| preserveAspectRatio | whether the aspect ratio of the pattern should be preserved |



By default, the pattern will be tiled by setting the middle of the pattern unit at the top left corner of the shape. 

## Example pattern with objectBoundingBox units
    <svg width="400" height="400">
    <defs>
      <pattern id="pattern1" width="0.2" height="0.2" patternUnits="objectBoundingBox">
          <circle cx="10" cy="10" r="10" fill="#0000ff" />
      </pattern>
    </defs>
    
    <rect x="10" y="10" width="100" height="100" stroke="black" fill="url(#pattern1)"/>
    </svg>



## Pattern coverage with combinations of patternUnits and patternContentUnits
SVG Patterns behave significantly differently than CSS background images when filling equivalent shapes. This can lead to significant surprises for new-comers to SVG. Below are examples of a pattern defined in all possible combinations of patternUnits and patternContentUnits - showing how these settings affect fill behavior.

[![enter image description here][1]][1]

    <svg width="800px" height="800px">
    <defs>
    <pattern id="pattern1" x="0" y="0" width="0.2" height="0.2"  patternUnits="objectBoundingBox" patternContentUnits="userSpaceOnUse">
          <circle cx="10" cy="10" r="10" fill="blue" />
      </pattern>
      
        <pattern id="pattern2" x="10" y="10" width="20" height="20"  patternUnits="userSpaceOnUse" patternContentUnits="objectBoundingBox">
          <circle cx=".1" cy=".1" r="0.1" fill="blue" />
      </pattern>
      
        <pattern id="pattern3" x="10" y="10" width="20" height="20"  patternUnits="userSpaceOnUse" patternContentUnits="userSpaceOnUse">
          <circle cx="10" cy="10" r="10" fill="blue" />
      </pattern>
      
        <pattern id="pattern4" x="0" y="0" width="0.2" height="0.2"  patternUnits="objectBoundingBox" patternContentUnits="objectBoundingBox">
          <circle cx=".1" cy=".1" r="0.1" fill="blue" />
      </pattern>
    </defs>
    
    <rect x="10" y="10" width="100" height="100" stroke="black" fill="url(#pattern1)"/>
    <rect x="150" y="10" width="200" height="150" stroke="black" fill="url(#pattern1)"/>
      <text x="10" y="200">patternUnits="objectBoundingBox" (20% of shape)</text> 
      <text x="10" y="220">patternContentUnits="userSpaceOnUse" (20px circle) </text>
      <text x="10" y="240" stroke="blue" stroke-width="1">(Units used by default)</text>
      
    <rect x="10" y="310" width="100" height="100" stroke="black" fill="url(#pattern3)"/>
    <rect x="150" y="310" width="200" height="150" stroke="black" fill="url(#pattern3)"/>
      <text x="10" y="500">patternUnits="userSpaceOnUse" (10px square box)</text> 
      <text x="10" y="520">patternContentUnits="userSpaceOnUse" (20px circle) </text>
      
    <rect x="410" y="10" width="100" height="100" stroke="black" fill="url(#pattern2)"/>
    <rect x="550" y="10" width="200" height="150" stroke="black" fill="url(#pattern2)"/>
      <text x="410" y="200">patternUnits="userSpaceOnUse" (10px square box)</text> 
      <text x="410" y="220">patternContentUnits="objectBoundingBox"(radius="10%") </text>
      
    <rect x="410" y="310" width="100" height="100" stroke="black" fill="url(#pattern4)"/>
    <rect x="550" y="310" width="200" height="150" stroke="black" fill="url(#pattern4)"/>
      <text x="410" y="500">patternUnits="objectBoundingBox" (20% of shape)</text> 
      <text x="410" y="520">patternContentUnits="objectBoundingBox"(radius="10%")  </text>
      
    </svg>


  [1]: http://i.stack.imgur.com/k2t7Z.png

## patternTransform examples
    <svg width="800px" height="800px">
    <defs>
    <pattern id="pattern1" x="0" y="0" width="0.2" height="0.2" >
          <circle cx="10" cy="10" r="10" fill="blue" />
      </pattern>
      
      <pattern id="pattern2" x="0" y="0" width="0.2" height="0.2" patternTransform="scale(1.5)">
          <circle cx="10" cy="10" r="10" fill="blue" />
      </pattern>
      
      <pattern id="pattern3" x="0" y="0" width="0.2" height="0.2"   patternTransform="skewX(45)">
          <circle cx="10" cy="10" r="10" fill="blue" />
      </pattern>
      
      <pattern id="pattern4" x="0" y="0" width="0.2" height="0.2"  patternTransform="matrix(1.5,-.70,.10,1.1,-30,10)">
          <circle cx="10" cy="10" r="10" fill="blue" />
      </pattern>
      
    
    </defs>
    
    <rect x="10" y="10" width="100" height="100" stroke="black" fill="url(#pattern1)"/>
    <rect x="150" y="10" width="200" height="150" stroke="black" fill="url(#pattern1)"/>
      <text x="10" y="200">Original</text> 
      
    <rect x="410" y="10" width="100" height="100" stroke="black" fill="url(#pattern2)"/>
    <rect x="550" y="10" width="200" height="150" stroke="black" fill="url(#pattern2)"/>
      <text x="410" y="200">patternTransform="scale(1.5)"</text> 
      
    <rect x="10" y="310" width="100" height="100" stroke="black" fill="url(#pattern3)"/>
    <rect x="150" y="310" width="200" height="150" stroke="black" fill="url(#pattern3)"/>
      <text x="10" y="500">patternTransform="skewX(45)"</text> 
      
    <rect x="410" y="310" width="100" height="100" stroke="black" fill="url(#pattern4)"/>
    <rect x="550" y="310" width="200" height="150" stroke="black" fill="url(#pattern4)"/>
      <text x="410" y="500">patternUnits="matrix(1.5,-.70,.10,1.1,-30,10)"</text> 
      
    </svg>

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/HOGd6.png

