---
title: "Opacity"
slug: "opacity"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Syntax
 - opacity: number (* strictly between 0 and 1) | inherit | initial |
   unset;

If you do not want apply opacity, you can use this instead:

[background: rgba(255, 255, 255, 0.6);][1]


  [1]: https://www.wikiod.com/css/backgrounds#Background Color with Opacity

Resources:

 - MDN: https://developer.mozilla.org/en/docs/Web/CSS/opacity;
 - W3C Transparency: the ‘opacity’ property:
   https://www.w3.org/TR/css3-color/#transparency
 - Browser support: http://caniuse.com/#feat=css-opacity

## Opacity Property
An element's opacity can be set using the `opacity` property. Values can be anywhere from `0.0` (transparent) to `1.0` (opaque).

**Example Usage**

    <div style="opacity:0.8;">
        This is a partially transparent element
    </div>

| Property Value | Transparency |
| ------ | ------ |
| `opacity: 1.0;`   | Opaque   |
| `opacity: 0.75;`   | 25% transparent (75% Opaque)   |
| `opacity: 0.5;`   | 50% transparent (50% Opaque)   |
| `opacity: 0.25;`   | 75% transparent (25% Opaque)   |
| `opacity: 0.0;`   | Transparent   |

## IE Compatibility for `opacity`
To use `opacity` in all versions of IE, the order is:

<!-- language: lang-css -->

    .transparent-element {
      /* for IE 8 & 9 */
      -ms-filter:"progid:DXImageTransform.Microsoft.Alpha(Opacity=60)"; // IE8
      /* works in IE 8 & 9 too, but also 5, 6, 7 */
      filter: alpha(opacity=60); // IE 5-7
      /* Modern Browsers */
      opacity: 0.6;
    }

