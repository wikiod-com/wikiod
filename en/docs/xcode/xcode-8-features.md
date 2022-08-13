---
title: "Xcode 8 features"
slug: "xcode-8-features"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This only works with projects using Swift 3+

## Color and image literals
Xcode 8 will automatically recognize any images you’ve got in an Asset Catalog and offer them up as a suggestion inside of a UIImage initializer.

So you could basically declare a new variable and then add an asset name that you have added to your asset catalog. For example `let img = dog`. `img` does now contain the image of `dog` that´s in the asset catalog.

Under the hood it’s creating code that looks like this: #imageLiteral(resourceName: "dog.png"). But inline in the source editor, you’ll just see the file name of the image.

So you could do this now `imageView.image = img`.

**Note that you need to click on the instellisense suggestion so that you see a thumbnail of the image in the code and then the image name.**

