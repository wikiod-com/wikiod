---
title: "Video aspect ratios"
slug: "video-aspect-ratios"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Aspect ratios are often expressed as a width:height ratio which is often - but not always - simplified and sometimes also as a simple floating point integer.

All of the following aspect ratios are the same value expressed in different ways:

* 1280:720
* 16:9
* 1.7777777777777777777777777777778

## Display aspect ratio (DAR)
[![enter image description here][2]][2]

This is a screenshot of a video playing. You see a normal 16:9 video like you would expect to see in any modern video solution. This - the aspect ratio that the viewer sees - is what is called the *display aspect ratio* or DAR.

From the illustrated parameters, we see that DAR = 1280:720 = 16:9 = 1.7777777777777777777777777777778.

  [2]: http://i.stack.imgur.com/f8cA4.jpg

## Picture aspect ratio (PAR)
Internally, all videos are a just series of pictures. Let's take a look at one such picture.

[![enter image description here][1]][1]

That looks odd, right? Indeed. The pictures that make up a video may have an aspect ratio that are different from the DAR, most often for algorithmic reasons (e.g. only sizes that are a multiple of 16 can be compressed by the chosen algorithm). This is called *picture aspect ratio* or PAR.

In this example, we have the picture dimensions exactly reversed between the displayed form (16:9) and the actual picture, so the PAR is 9:16. Normally, the differences are smaller but this example is exaggerated for clarity.

From the illustrated parameters, we see that PAR = 720:1280 = 9:16 = 0.5625

  [1]: http://i.stack.imgur.com/MDFeG.jpg

## Sample aspect ratio (SAR)
As the *picture aspect ratio* example indicates, videos are series of pictures that do not necessarily have the same aspect ratio as the final result to be displayed to the user.

So how do you get from those stretched pictures to the normally displayed output? You need a stretching factor! This stretching factor is applied to the picture in order to bring it to the correct aspect ratio for display. This is the *sample aspect ratio* or SAR.

[![enter image description here][1]][1]

The stretching factor is often expressed as a ratio of two integers. You calculate it as SAR = PAR / DAR.

From the illustrated parameters, we see that SAR = 9:16 / 16:9 = (9/16)/(16/9) = 81/256 = 3.1604938271604938271604938271605

  [1]: http://i.stack.imgur.com/fm4a4.png

## Pixel aspect ratio
This is another name for *sample aspect ratio* and should be avoided, as the natural acronym (PAR) conflicts with *picture aspect ratio*.

