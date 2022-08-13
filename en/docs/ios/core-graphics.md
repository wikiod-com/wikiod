---
title: "Core Graphics"
slug: "core-graphics"
draft: false
images: []
weight: 9926
type: docs
toc: true
---

## Creating a Core Graphics Context
> # Core Graphics context
> A Core Graphics context is a canvas which we can draw in it and set some properties like the line thickness.

# Making a context

To make a context, we use the `UIGraphicsBeginImageContextWithOptions()` C function. Then, when we are done with drawing, we just call `UIGraphicsEndImageContext()` to end the context:

## Swift

    let size = CGSize(width: 256, height: 256)

    UIGraphicsBeginImageContextWithOptions(size, false, 0)

    let context = UIGraphicsGetCurrentContext()

    // drawing code here

    UIGraphicsEndImageContext()

## Objective-C

    CGSize size = [CGSize width:256 height:256];

    UIGraphicsBeginImageContextWithOptions(size, NO, 0);

    CGContext *context = UIGraphicsGetCurrentContext();

    // drawing code here

    UIGraphicsEndImageContext();

In the code above, we passed 3 parameters to the `UIGraphicsBeginImageContextWithOptions()` function:

1. A `CGSize` object which stores the whole size of the context (the canvas)

2. A boolean value which if it is true, the context will be opaque

3. An integer value which sets the scale (1 for non-retina, 2 for retina and 3 for retina HD screens). If set to 0, the system automatically handles the scale based on the target device.

## Presenting the Drawn Canvas to User


