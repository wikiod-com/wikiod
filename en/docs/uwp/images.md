---
title: "Images"
slug: "images"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Assigning a BitmapImage to Image's Source
    Image img = new Image();
    BitmapImage bitmap = new BitmapImage(new Uri("ms-appx:///Path-to-image-in-solution-directory", UriKind.Absolute)); 
    img.Source = bitmap;

