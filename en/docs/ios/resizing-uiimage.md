---
title: "Resizing UIImage"
slug: "resizing-uiimage"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Parameters
| CGInterpolationQuality | Levels of interpolation quality for rendering an image. |
| ------ | ------ |
|  Interpolation quality is a graphics state parameter |  typedef enum CGInterpolationQuality CGInterpolationQuality;
 |

## Resize any image by size & quality
    - (UIImage *)drawImageBySize:(CGSize)size quality:(CGInterpolationQuality)quality
    {
        UIGraphicsBeginImageContextWithOptions(size, NO, 0.0);
        CGContextRef context = UIGraphicsGetCurrentContext();
        CGContextSetInterpolationQuality(context, quality);
        [self drawInRect: CGRectMake (0, 0, size.width, size.height)];
        UIImage *resizedImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        return resizedImage;
    }

