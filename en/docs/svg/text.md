---
title: "Text"
slug: "text"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
| `<text>`| Details |
| --- | --- |
| x | The x position of the text. |
| y | The y position of the text. |
| dx | Relative shift in x position. |
| dy | Relative shift in y position. |
| rotate | Specifies the angular displacement for text glyphs. |
| textLength | Fits the text into the given length. |
| lengthAdjust | Specifies whether just kerning or kerning & glyphs are compressed/stretched to fit text into textLength specified. Values: spacing or spacingAndGlyphs |
| -- | **Parameters common to all text chunking elements (text, tref, textPath, tspan)**|
| text-anchor | Specifies horizontal alignment. Values: start, middle, end. |
| baseline-shift | Shifts text baseline based on either values provided by the font table for superscript or subscript positioning (sub, super) or by a positive or negative % or length. Values: sub, super, %, or length. |

baseline-shift is not supported by the most current versions of Firefox and Microsoft browsers as of July 2016.

## Draw text
    
    <svg xmlns="http://www.w3.org/2000/svg">
        <text x="40" y="60" font-size="28">Hello World!</text>
    </svg>

The x and y coordinate specifies the position of the bottom-left corner of the text (unless text-anchor has been modified).

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/QXs8r.png

## Super- and subscript
Using the baseline-shift parameter, you can specify super- or subscript. But this is not supported by all major browsers.

    <svg xmlns="http://www.w3.org/2000/svg">
        <text x="10" y="20">x<tspan baseline-shift="super">2</tspan></text>
        <text x="10" y="60">f<tspan baseline-shift="sub">x</tspan></text>
    </svg>

For a cross-browser solution, you can use dy, dx and relative font size.

    <svg xmlns="http://www.w3.org/2000/svg">
        <text x="10" y="40">x<tspan dy="-7" font-size=".7em">2</tspan></text>
        <text x="10" y="80">f<tspan dy="3" font-size=".7em">x</tspan></text>
    </svg>

## Rotate text
The rotate property rotates each character by the specified angle.

    <svg xmlns="http://www.w3.org/2000/svg">
        <text x="10" y="20" rotate="30">Each character is rotated</text>
    </svg>

To rotate the whole text element, you have to use the transform property.

    <svg xmlns="http://www.w3.org/2000/svg">
        <text transform="translate(10, 60) rotate(30)">The whole text is rotated</text>
    </svg>

## Individual Letter Positioning With Arrays of X & Y Values
    <svg width="400px" height="200px"> 
     <text x="1em, 2em, 3em, 4em, 5em" y="3em, 4em, 5em">
        Individually Spaced Text
      </text>
    </svg>

The Text element supports individual placement of letters by accepting an array of values for x and y.

