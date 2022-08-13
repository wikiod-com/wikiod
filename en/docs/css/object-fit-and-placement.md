---
title: "Object Fit and Placement"
slug: "object-fit-and-placement"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

The properties `object-fit` and `object-position` are not supported by Internet Explorer.

## object-fit
<!-- language-all: lang-css -->

The **object-fit** property will defines how an element will fit into a box with an established height and width. Usually applied to an image or video, Object-fit accepts the following five values:

**FILL**

 
    object-fit:fill;

[![object-fit:fill;][1]][1]

Fill stretches the image to fit the content box without regard to the image's original aspect ratio.

**CONTAIN**

    object-fit:contain;
 
[![object-fit:contain;][2]][2]

Contain fits the image in the box's height or width while maintaining the image's aspect ratio.

**COVER**

    object-fit:cover;
[![object-fit:cover;][3]][3]

Cover fills the entire box with the image. The image aspect ratio is preserved, but the image is cropped to the dimensions of the box.

**NONE**

    object-fit:none;
[![object-fit:none;][4]][4]

None ignores the size of the box and is not resized. 

**SCALE-DOWN**

    object-fit:scale-down;

Scale-down either sizes the object as `none` or as `contain`. It displays whichever option results in a smaller image size.
[![object-fit:scale-down;][5]][5]


  [1]: http://i.stack.imgur.com/xIdvn.png
  [2]: http://i.stack.imgur.com/qpiUd.png
  [3]: http://i.stack.imgur.com/zxl94.png
  [4]: http://i.stack.imgur.com/YdXVL.png
  [5]: http://i.stack.imgur.com/bnDKA.png

