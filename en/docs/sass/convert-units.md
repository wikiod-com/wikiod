---
title: "Convert units"
slug: "convert-units"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Convert px to (r)em
To convert px to em or rem you can use the following function:

    @function rem-calc($size, $font-size : $font-size) {
        $font-size: $font-size + 0px;
        $remSize: $size / $font-size;
        @return #{$remSize}rem;
    }

    @function em-calc($size, $font-size : $font-size) {
        $font-size: $font-size + 0px;
        $remSize: $size / $font-size;
        @return #{$remSize}em;
    }

The `$font-size` is the original font size.

For example:

    $font-size: 14;
    
    body {
      font-size: #{$font-size}px;
      font-size: rem-calc(14px); // returns 1rem
      // font-size: rem-calc(28); // returns 2rem
    }

