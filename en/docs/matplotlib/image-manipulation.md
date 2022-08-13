---
title: "Image manipulation"
slug: "image-manipulation"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Opening images
Matplotlib includes the `image` module for image manipulation
    
    import matplotlib.image as mpimg
    import matplotlib.pyplot as plt

Images are read from file (`.png` only) with the `imread` function:

    img = mpimg.imread('my_image.png')

and they are rendered by the `imshow` function:

    plt.imshow(img)

Let's _plot_ the [Stack Overflow logo][1]:

    import matplotlib.image as mpimg
    import matplotlib.pyplot as plt
    img = mpimg.imread('so-logo.png')
    plt.imshow(img)
    plt.show()

The resulting plot is
[![Simple plot of the SO logo][2]][2]


  [1]: http://stackoverflow.com/company/logos
  [2]: http://i.stack.imgur.com/IL8dD.png

