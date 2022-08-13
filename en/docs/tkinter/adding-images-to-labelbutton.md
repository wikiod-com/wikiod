---
title: "Adding Images To LabelButton"
slug: "adding-images-to-labelbutton"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

This shows the proper usage of images and how to correctly display images.

## File Formats Supported By Tkinter
Tkinter support .ppm files from PIL(Python Imaging Library), .JPG, .PNG and .GIF.

To import and image you first need to create a reference like so:

    Image = PhotoImage(filename = [Your Image here])

Now, we can add this image to Button and Labels like so using the "img" callback:

     Lbl = Label (width=490, img=image)

 





## Usage of .GIF formats.
In order to display a gif, you need to show it frame by frame sort of like an animation.

An animated gif consists of a number of frames in a single file. Tk loads the first frame but you can specify different frames by passing an index parameter when creating the image. For example:

    frame2 = PhotoImage(file=imagefilename, format="gif -index 2")

If you load up all the frames into separate PhotoImages and then use timer events to switch the frame being shown (label.configure(image=nextframe)). The delay on the timer lets you control the animation speed. There is nothing provided to give you the number of frames in the image other than it failing to create a frame once you exceed the frame count.



