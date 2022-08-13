---
title: "clipPath"
slug: "clippath"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Parameters
| Parameter | Description |
| ------ | ------ |
| clipPathUnits   | the coordinate system of the pattern contents either *objectBoundingBox* or *userSpaceOnUse*

[Related W3C Recommendation informations](https://www.w3.org/TR/SVG/masking.html#clipPath-geometry)

## Clipping to a circle path
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" xmlns:xlink="http://www.w3.org/1999/xlink">
      <defs>
        <clipPath id="circleClip">
          <circle cx="50" cy="60" r="20" />
        </clipPath>
      </defs>
      <image width="100" height="100" style="clip-path:url(#circleClip)" xlink:href="https://cdn.sstatic.net/Sites/stackoverflow/company/img/logos/so/so-icon.png" />
    </svg>

  | Original logo | Logo after clipping |
  |---|---|
  | ![logo](http://i.stack.imgur.com/9bEcW.png) | ![logo after clipping](http://i.stack.imgur.com/LQDtO.png) |

