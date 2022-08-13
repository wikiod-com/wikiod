---
title: "Images"
slug: "images"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Images can be added to an email as foreground or background images. Images should be remotely hosted on a web server and pulled into an email using absolute paths. While similar to the web, the syntax and considerations can be a little different.

## Parameters
| Parameters | Details |
| --------- | ------- |  
| `align` | Specifies the alignment of an image relative to the surrounding element(s) |
| `alt` | Alternative text that should be displayed if for some reason the image could not be displayed |
| `border` | How the element handles crossorigin requests |
| `height` | Specifies the height of the image (optional) |
| `src` | Specifies the URL of the image |
| `srcset` | Images to use in different situations (e.g., high-resolution displays, small monitors, etc) | 
| `style` | Specifies inline styles to be applied to an element | 
| `usemap` | Name of image map to use |
| `width` | Specifies the width of the image |


## Foreground Images
Basic foreground images can be inserted into an email document [just like they are for web pages](https://www.wikiod.com/html/images):

    <img src="http://www.yourserver.com/email/images/logo.png">

And they can be made links by wrapping them in an `<a href>` tag:

    <a href="http://www.yourwebsite.com">
        <img src="http://www.yourserver.com/email/images/logo.png">
    </a>

Email `<img>`s use many of the same attributes as on the web, including some that are _deprecated on the web_, are included at the bottom of this page under **Parameters**. 

---

✖ **don't** use relative image paths:

    <img src="images/logo.gif">

✖ It's advisable **not to use** base64. It's not supported in Outlook and can quick bloat the file size of an email, which triggers spam filters.

    <img src="data:image/png;base64,iVBOR..." alt="Your Image">


