---
title: "Getting started with imagemagick"
slug: "getting-started-with-imagemagick"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Convert an image between file formats
Using the `magick` command (or `convert for IM 6.x users) you con convert any image format to any other. with no other arguments, as little processing as possible will be done to move from one format to the other. Simply specify your input and output files. To convert a JPEG to a PNG:

    $ magick image.jpg image.png

Or a TIFF to a GIF:

    $ magick image2.tif image.gif

## Create an animated gif
Starting from a sequence of static images (for example called *frame01.jpg*, *frame02.jpg* and so on) an animated gif can be created using the following command:

    magick -delay 10 -loop 0 frame*.jpg animation.gif

* `-delay 10` sets the interval between the frames to 0.1 seconds

* `-loop 0` creates an infinite looping animation 

## Installation or Setup
You can install ImageMagick from source or Binary.

**In case of Windows Binary**

Download executable binary file. And simply click on the appropriate version and it will launch itself and follow the wizard.
You can type the following command to find out whether ImageMagick is successfully installed or not:

    identify -version

[Download a ready-to-run ImageMagick and installation guide](https://www.imagemagick.org/script/download.php)

[Download source file and installation guide](https://www.imagemagick.org/script/install-source.php)

## Compare the difference between an image
ImageMagick includes a number of command-line utilities for manipulating images.
Here we will use `compare` command-line tool. 

> `compare` tool is very useful. Suppose you want to test (e.g. layout,
> color, icons etc.) the difference between your expected design UI HTML
> file with actual result of JSP file, you just need to use `compare` command.
> You do not need to compare with your own eyes.

**1.jpg**

[![enter image description here][1]][1]

**2.jpg**

[![enter image description here][2]][2]

First we will compare **same image**.

    magick compare 1.jpg 1.jpg difference1.jpg

**Result ⇒　difference1.jpg**

White de-emphasizes (lowlight) pixels that are untouched by modifying.

[![enter image description here][3]][3]


Let's compare an image to one thats been modifed. 

    magick compare 1.jpg 2.jpg difference2.jpg

**Result ⇒　difference2.jpg** 

The red areas of the difference image emphasizes (highlight) pixels that are affected by the image modifying.

[![enter image description here][4]][4]

Here is the list of ImageMagick [command-line utilities](https://www.imagemagick.org/script/command-line-tools.php)


  [1]: https://i.stack.imgur.com/CzQpc.jpg
  [2]: https://i.stack.imgur.com/dPwKl.jpg
  [3]: https://i.stack.imgur.com/atgxA.jpg
  [4]: https://i.stack.imgur.com/BFJaJ.jpg

