---
title: "Colours in Processing"
slug: "colours-in-processing"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This article will show you the various colours formats in Processing and the ways in which they can be used.

## Syntax
 - color(r, g, b);
 - color(r, g, b, alpha);
 - color(gray);
 - color(gray, alpha);
 - color(h, s, l); //The mode must be HSB. You can change this using colorMode.

## Parameters
| Parameters | Details |
| ------ | ------ |
| r   | Is the red of the color when the mode is `RGB`.    |
| g | Is the green of the color when the mode is `RGB`. |
| b | Is the blue of the color when the mode is `RGB`. |
| alpha | Is the opacity of the color. |
| h | The hue of the color when the mode is `HSB`. |
| s | The saturation of the color when the mode is `HSB`. |
| l | The brightness/lightness of the color when the mode is `HSB`. |
| gray | The value between black (being 0) and white (being 255). |

Although it has not been mentioned in the official Processing documentation, there is a `CMYK` mode which you can use.

## Colour Notation
There are various ways to use colours in Processing since Processing is very flexible with colour formats.

**RGB and RGBA**

This is the standard RGB(A) notation and the default color mode. The first three colour values (red, green, blue) range from `0` to `255`. For example, the below example is the colour red since red is maxed out at `255` while the others colours are at `0`. White is at `(255, 255, 255)` and black is `(0, 0, 0)`. The optional 4th parameter indicates the alpha value -- i.e. transparency. As in other the components, the range of values is again [0-255]; `0` being completely transparent and `255` being completely solid. 

    color(255, 0, 0) // This is red

    color(0, 255, 0, 255) // This is opaque green, and is the same as color(0, 255, 0)

    color(255, 255, 0, 10) // This is almost transparent yellow
**HSB**

HSB notation is similar to RGB notation, except for the fact that red, green, and blue are replaced with hue, saturation, and brightness, respectively. You can switch into HSB by using `colorMode(HSB)`.

    color(0, 0, 255) //This is white
As with RGB, HSB also has the alpha value as the fourth parameter.

**Gray Values**

If one parameter is specified to a color function, it will be interpreted as the amount between black and white. White is represented as 255, and black as 0. It is the same as `color(param1, param1, param1)` in RGB mode. If two parameters are specified, then the first parameter will be interpreted as above and the second will be the alpha value.

