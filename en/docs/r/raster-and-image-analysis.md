---
title: "Raster and Image Analysis"
slug: "raster-and-image-analysis"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

See also [I/O for Raster Images][1]


  [1]: https://www.wikiod.com/r/io-for-raster-images

## Calculating GLCM Texture
[Gray Level Co-Occurrence Matrix][1] (Haralick et al. 1973) texture is a powerful image feature for image analysis. The `glcm` package provides a easy-to-use function to calculate such texutral features for `RasterLayer` objects in R. 

    library(glcm)
    library(raster)

    r <- raster("C:/Program Files/R/R-3.2.3/doc/html/logo.jpg")
    plot(r)
[![enter image description here][2]][2]

**Calculating GLCM textures in one direction**

    rglcm <- glcm(r, 
                  window = c(9,9), 
                  shift = c(1,1), 
                  statistics = c("mean", "variance", "homogeneity", "contrast", 
                                 "dissimilarity", "entropy", "second_moment")
                  )
    
    plot(rglcm)
[![enter image description here][3]][3]

**Calculation rotation-invariant texture features**

The textural features can also be calculated in all 4 directions (0°, 45°, 90° and 135°) and then combined to one rotation-invariant texture. The key for this is the `shift` parameter:
 

    rglcm1 <- glcm(r, 
                  window = c(9,9), 
                  shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
                  statistics = c("mean", "variance", "homogeneity", "contrast", 
                                 "dissimilarity", "entropy", "second_moment")
                  )
    
    plot(rglcm1)
[![enter image description here][4]][4]


  [1]: https://en.wikipedia.org/wiki/Co-occurrence_matrix
  [2]: http://i.stack.imgur.com/yBLGi.png
  [3]: http://i.stack.imgur.com/YBnub.png
  [4]: http://i.stack.imgur.com/U0SHY.png

## Mathematical Morphologies
The package `mmand` provides functions for the calculation of Mathematical Morphologies for n-dimensional arrays. With a little workaround, these can also be calculated for raster images. 

    library(raster)
    library(mmand)
    
    r <- raster("C:/Program Files/R/R-3.2.3/doc/html/logo.jpg")
    plot(r)

[![][1]][1]

At first, a kernel (moving window) has to be set with a size (e.g. 9x9) and a shape type (e.g. `disc`, `box` or `diamond`)

    sk <- shapeKernel(c(9,9), type="disc")

Afterwards, the raster layer has to be converted into an array wich is used as input for the `erode()` function. 

    rArr <- as.array(r, transpose = TRUE)
    rErode <- erode(rArr, sk)
    rErode <- setValues(r, as.vector(aperm(rErode)))

Besides `erode()`, also the morphological functions `dilate()`, `opening()` and `closing()` can be applied like this. 

    plot(rErode)
[![Eroded R Logo][2]][2]


  [1]: http://i.stack.imgur.com/cuCPz.png
  [2]: http://i.stack.imgur.com/mAuCt.png

