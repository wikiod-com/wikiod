---
title: "CSSHTML Support"
slug: "csshtml-support"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Not every css property or HTML tag is supported by all email clients and here we can keep track of it & possible work around.

## Visual Decorations
CSS properties which add visual decorations but are not supported can be replaced with image tag.

For example: `border-radius` is not supported in Yahoo! Mail, Outlook 2007/10/13 +, Outlook 03/Express/Mail & Android 4 (Gmail) +

To work around this we can add images with border radius in them i.e buttons with left & right side as image:

    <td><img src="img-border-left.png"><td>
    <td style="background: #000; color: #fff">Button Text</td>
    <td><img src="img-border-right.png"><td>

Here is a list of property that should be avoided and used as image instead:

`border-radius`, `text-shadow`, `box-shadow`, `text-fill-color` & `text-fill-stroke`

