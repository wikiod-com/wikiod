---
title: "NSFont"
slug: "nsfont"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

NSFont is the object that provides Mac applications with glyph information and font characteristics to be used for primarily for display. You'll learn how to create and use NSFont objects in a variety of ways, both common and uncommon.

## Creating an NSFont object
The preferred and most common way of making an NSFont object is the following:

Objective-C
-----------

    // Name is PostScript name of font; size is in points.
    NSFont *essayFont = [NSFont fontWithName:@"Times New Roman" size:12.0];



