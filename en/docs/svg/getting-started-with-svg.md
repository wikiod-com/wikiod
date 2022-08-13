---
title: "Getting started with SVG"
slug: "getting-started-with-svg"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Inline SVG
Inline SVG allows SVG markup, written within HTML, to generate graphics in the browser.

When using SVG inline, a DOCTYPE is not strictly required. Instead just the `<svg>` opening and closing tags together with either a [viewBox](https://www.wikiod.com/svg/the-svg-element#viewBox) or width and height attributes will suffice:

    <svg width="100%" height="100%">
        <!-- SVG elements go here -->
    </svg>

The `<svg>` fragment above acts as both a container and a structural element. This fragment establishes its own coordinate system.

Below is an example of rendering an SVG fragment with some content. It will produce a rectangle with "Hello World!" text within it.

    <svg width="50%" viewBox="0 0 10 10">
        <rect x="1" y="1" width="5" height="3" fill="teal" />
        <text x="2" y="2" font-family="Palatino, Georgia, serif" font-size="3%" font-weight="bold" fill="white">Hello World!</text>
    </svg>

Result:

[![Simple SVG example output][1]][1]


  [1]: http://i.stack.imgur.com/WWdXL.png

## SVG as an <img>
You can render the contents of an SVG file as an image within an HTML document using an `<img>` tag. For example:
 
    <img src="my_svg_file.svg" alt="Image description">

The dimensions of the image will, by default, display according to the **width** and **height** properties specified in the SVG file referenced in the `src` attribute.

It's worth noting various limitations inherent in this approach:

 - Browser support, whilst good, doesn't include Internet Explorer 8 and earlier versions, nor Android 2.3 and earlier versions.
 - You can't style the individual elements contained within the SVG file using CSS which is external to the SVG file. All CSS must be within the image file itself.
 - JavaScript won't run.
 - The image must be complete in a single file. For example, if the SVG file contains raster images then those internal images must be encoded as data URLs.

## SVG as a background image
You can display an SVG file within an HTML document, by specifying it as a background image in CSS. For example:

    .element {
        background-size: 100px 100px;
        background: url(my_svg_file.svg);
        height: 100px;
        width: 100px;
    }

If the dimensions specified in your SVG file are larger than the dimensions of your HTML element, it may be desirable to specify the `background-size` property, to scale the SVG to fit within its element.

As with using [SVG as an `<img>`][1], it's worth noting some limitations with this approach:
- Browser support doesn't include Internet Explorer 8 and earlier versions, nor Android 2.3 and earlier versions.
- You can't style the individual elements contained within the SVG file using CSS which is external to the SVG file. All CSS must be within the image file itself.


  [1]: https://www.wikiod.com/svg/getting-started-with-svg#SVG as an <img>

