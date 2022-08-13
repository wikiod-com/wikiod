---
title: "IO for raster images"
slug: "io-for-raster-images"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

See also [Raster and Image Analysis][1] and [Input and Output][2]


  [1]: https://www.wikiod.com/r/raster-and-image-analysis
  [2]: https://www.wikiod.com/r/input-and-output

## Load a multilayer raster
The R-Logo is a multilayer raster file (red, green, blue)

    library(raster)
    r <- stack("C:/Program Files/R/R-3.2.3/doc/html/logo.jpg")
    plot(r)

[![][1]][1]

The individual layers of the `RasterStack` object can be adressed by `[[`. 

    plot(r[[1]])
[![][2]][2]


  [1]: http://i.stack.imgur.com/9jVrN.png
  [2]: http://i.stack.imgur.com/tOgqL.png

