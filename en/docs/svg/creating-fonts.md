---
title: "Creating fonts"
slug: "creating-fonts"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

SVG Fonts are not directly supported by Browsers any more. Still they are very convenient for programmatically generating fonts such as symbol fonts or barcode fonts.

There are many tools out there that allow you to convert svg fonts to any other font format. 

Here is a list of tools you can use with SVG fonts.

# Converters

* https://github.com/fontello/svg2ttf


## a simple font
A simple example of an svg font. A few things to note here:

* the coordinate system of the glyphs is in opposition to the usual coordinate system in svg. **the y axis points upwards**. The point 0,0 is in the lower right corner.
* All paths in a font must be drawn counter clockwise.
* In most tools only the d attribute of the glyph element is supported. Child elements won't work, though they are technically allowed.


    <svg xmlns="http://www.w3.org/2000/svg">
      <font id = "myFont"
            horiz-adv-x   = "1000"
            vert-origin-x = "0"
            vert-origin-y = "0" >
        <font-face font-family  = "myFont"
                   font-weight  = "normal"
                   units-per-em = "1000">
          <font-face-src>
            <font-face-name name="myFont"/>
          </font-face-src>
        </font-face>`
        <glyph unicode="a" d="M0 0 H1000 L500 1000z M200 200 L500 800 L800 200z" />
        <glyph unicode="b" d="M0 0 H1000 L500 1000z M200 200 L500 800 L800 200z" />
      </font>
    </svg>

If you have wider or narrower glyphs, just change the horiz-adv-x on the glyph element itself.

    <glyph unicode="a" horiz-adv-x="512" d="M0 0 H1000 L500 1000z M200 200 L500 800 L800 200z" />
    
## font picking

the unicode property is used for later glyph selection. You can use simple letters or unicode codepoints as well as ligatures (combination of letters or unicode codepoints)

* `unicode="abc"`
* `unicode="&#97;&#98;"`
* `unicode="ab&#97;&#98;"`
* `unicode="a"`
* `unicode="&#98;"`

**glyphs are always selected by first match, so do have all ligatures before any single character.**

unicode codepoints can be written in decimal `&#123;` or in hexadecimal `&#x1f` notation.

## ascent, descent and baseline
the units-per-em property is one of the most important font properties. It's used to give any value of any other property any meaning. 

the CSS2 font spec has a nice [definition of the em sqare][1]:

> Certain values, such as width metrics, are expressed in units that are relative to an abstract square whose height is the intended distance between lines of type in the same type size

the **default baseline is at 0** in the em square. for line-height calculation and alignement purposes the two prperties ascent and descent are of most importance.

Ascent is the maximum distance from the baseline to the highest point of your largest glyph. Usaually that is 1em, so the value you gave for units-per-em.

Descent is the maximum distance from the baseline to the lowest point in any glyph of your font.

Here is a font with glyphs rendering a line at the lowest and highest point as well as at the baseline.

    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1000 1000">
      <font id = "myFont"
            horiz-adv-x   = "1000"
            vert-origin-x = "0"
            vert-origin-y = "0" >
        <font-face font-family  = "myFont"
                   font-weight  = "normal"
                   units-per-em = "1000"
                   descent="500"
                   ascent="1000">
          <font-face-src>
            <font-face-name name="myFont"/>
          </font-face-src>
        </font-face>`
        <glyph unicode = "a" d = "M0 900h1000v100h-1000z" />
        <glyph unicode = "b" d = "M0 0h1000v100h-1000z" />
        <glyph unicode = "c" d = "M0 -500h1000v100h-1000z" />
      </font>
    </svg>

Ascent and descent are used to determine the line-height.
Units-per-em and baseline are used to determine the position and size relative to other fonts used.. 

  [1]: https://www.w3.org/TR/2008/REC-CSS2-20080411/fonts.html#emsq

