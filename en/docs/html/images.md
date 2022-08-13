---
title: "Images"
slug: "images"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

## Syntax
- `<img src="" alt="">`

## Parameters
| Parameters | Details |
| --------- | ------- |  
| `src` | Specifies the URL of the image |
| `srcset` | Images to use in different situations (e.g., high-resolution displays, small monitors, etc) | 
| `sizes`| Image sizes between breakpoints | 
| `crossorigin` | How the element handles crossorigin requests |
| `usemap` | Name of image map to use |
| `ismap` | Whether the image is a server-side image map | 
| `alt` | Alternative text that should be displayed if for some reason the image could not be displayed |
| `width` | Specifies the width of the image (optional) |
| `height` | Specifies the height of the image (optional) |

## Creating an image
To add an image to a page, use the image tag. 

Image tags (`img`) do not have closing tags. The two main attributes you give to the `img` tag are `src`, the image source and `alt`, which is alternative text describing the image.

    <img src="images/hello.png" alt="Hello World">

You can also get images from a web URL:
    
    <img src="https://i.stack.imgur.com/ALgZi.jpg?s=48&g=1" alt="StackOverflow user Caleb Kleveter">

Note: Images are not technically inserted into an HTML page, images are linked to HTML pages. The `<img>` tag creates a holding space for the referenced image.

It is also possible to embed images directly inside the page using base64:
 
    <img src="data:image/png;base64,iVBOR..." alt="Hello World">

Tip: To link an image to another document, simply nest the `<img>` tag inside `<a>` tags.

## Choosing alt text
Alt-text is used by screen readers for visually impaired users and by search engines. It's therefore important to write good alt-text for your images.

The text should look correct even if you replace the image with its alt attribute. For example:

<!-- language: lang-html -->
```
<!-- Incorrect -->
<img src="anonymous.png" alt="Anonymous user avatar"/> An anonymous user wrote:
<blockquote>Lorem ipsum dolor sed.</blockquote>
<a href="https://google.com/"><img src="edit.png" alt="Edit icon"/></a> /
<a href="https://google.com/"><img src="delete.png" alt="Delete icon"/></a>
```
  
Without the images, this would look like:

> Anonymous user avatar An anonymous user wrote:
> <blockquote>Lorem ipsum dolor sed.</blockquote>
> <a href="https://google.com/">Edit icon</a> /
> <a href="https://google.com/">Delete icon</a>

To correct this:
* Remove the alt-text for the avatar. This image adds information for sighted users (an easily identifiable icon to show that the user is anonymous) but this information is already available in the text.<sup>1</sup>
* Remove the "icon" from the alt-text for the icons. Knowing that this would be an icon if it were there does not help to convey its actual purpose.

<!-- language: lang-html -->
```
<!-- Correct -->
<img src="anonymous.png" alt=""/> An anonymous user wrote:
<blockquote>Lorem ipsum dolor sed.</blockquote>
<a href="https://google.com/"><img src="edit.png" alt="Edit"/></a> /
<a href="https://google.com/"><img src="delete.png" alt="Delete"/></a>
```

> An anonymous user wrote:
> <blockquote>Lorem ipsum dolor sed.</blockquote>
> <a href="https://google.com/">Edit</a> /
> <a href="https://google.com/">Delete</a>

<h2>Footnotes</h2>

<sup>1</sup> There is a semantic difference between including an empty alt attribute and excluding it altogether. An empty alt attribute indicates that the image is *not* a key part of the content (as is true in this case - it's just an additive image that is not necessary to understand the rest) and thus may be omitted from rendering. However, the lack of an alt attribute indicates that the image *is* a key part of the content and that there simply is no textual equivalent available for rendering.

## Responsive image using the srcset attribute
Using srcset with sizes
===================================

    <img sizes="(min-width: 1200px) 580px,
          (min-width: 640px) 48vw,
          98vw"
        srcset="img/hello-300.jpg 300w,
          img/hello-600.jpg 600w,
          img/hello-900.jpg 900w,
          img/hello-1200.jpg 1200w"
        src="img/hello-900.jpg" alt="hello">

`sizes` are like media queries, describing how much space the image takes of the viewport.
- if viewport is larger than 1200px, image is exactly 580px (for example our content is centered in container which is max 1200px wide. Image takes half of it minus margins).
- if viewport is between 640px and 1200px, image takes 48% of viewport (for example image scales with our page and takes half of viewport width minus margins).
- if viewport is any other size , in our case less than 640px, image takes 98% of viewport (for example image scales with our page and takes full width of viewport minus margins). **Media condition must be omitted for last item.**

`srcset` is just telling the browser what images we have available, and what are their sizes.
- `img/hello-300.jpg` is 300px wide,
- `img/hello-600.jpg` is 600px wide,
- `img/hello-900.jpg` is 900px wide,
- `img/hello-1200.jpg` is 1200px wide

`src` is always mandatory image source. In case of using with `srcset`, `src` will serve fallback image in case browser is not supporting `srcset`.


Using srcset without sizes
==========================

    <img src="img/hello-300.jpg" alt="hello"
         srcset="img/hello-300.jpg 1x, 
           img/hello-600.jpg 2x,
           img/hello-1200.jpg 3x">

`srcset` provides list of available images, with device-pixel ratio `x` descriptor.
- if device-pixel ratio is 1, use `img/hello-300.jpg`
- if device-pixel ratio is 2, use `img/hello-600.jpg`
- if device-pixel ratio is 3, use `img/hello-1200.jpg`

`src` is always mandatory image source. In case of using with `srcset`, `src` will serve fallback image in case browser is not supporting `srcset`.



## Responsive image using picture element
**Code**

    <picture>
      <source media="(min-width: 600px)" srcset="large_image.jpg">
      <source media="(min-width: 450px)" srcset="small_image.jpg">
      <img src="default_image.jpg" style="width:auto;">
    </picture>

**Usage**

To display different images under different screen width, you must include all images using the source tag in a picture tag as shown in the above example.

**Result**

- On screens with screen width >600px, it shows large_image.jpg
- On screens with screen width >450px, it shows small_image.jpg
- On screens with other screen width, it shows default_image.jpg

## Image width and height
> **Note:** The **width and height attributes are *not* deprecated on images** and never have been. Their use has been made much stricter though.

The dimensions of an image can be specified using the `width` and `height` attributes of the image tag:

    <img src="images/200x200-img.png" width="200" height="200" alt="A 200x200 image">

By specifying the `width` and `height` of an image, your structure gives the browser a hint as to how the page should be laid out even if you are just specifying the actual image size. If the image dimensions are not specified, the browser will need to recalculate the layout of the page after the image is loaded, which may cause the page to "jump around" while it's loading.

<!-- if version [lte 4.1] -->

You can give the image a width and height in either the number of CSS pixels or a percentage of the image's actual dimensions.

These examples are all valid:

    <img src="images/50x50-img.png" width="50" height="50" alt="A 50x50 image">
    <img src="images/50x50-img.png" width="200" height="200" alt="A 200x200 image">
    <img src="images/200x200-img.png" width="50" height="50" alt="A 50x50 image">
    <img src="images/200x200-img.png" width="50%" height="50%" alt="A 100x100 image">

<!-- end version if -->

<!-- if version [gte 5] -->

The width and height of the image must be specified in CSS pixels; a percentage value is no longer a valid value. As well, if both attributes are specified, they must fit into [one of three formulas](https://www.w3.org/TR/html5/embedded-content-0.html#dimension-attributes) that maintain aspect ratio. Although valid, you should not use the width and height attributes to stretch an image to a larger size.

These examples are valid:

    <img src="images/50x50-img.png" width="50" height="50" alt="A 50x50 image">
    <img src="images/200x200-img.png" width="50" height="50" alt="A 50x50 image">
    <img src="images/50x50-img.png" width="0" height="0" alt="A hidden tracking image">

This example is not recommended:

    <img src="images/50x50-img.png" width="200" height="200" alt="A 200x200 image">

These examples are invalid:

    <img src="images/200x200-img.png" width="50%" height="50%" alt="A 100x100 image">
    <img src="images/200x200-img.png" width="1" height="200" alt="A 1x200 image">

<!-- end version if -->

