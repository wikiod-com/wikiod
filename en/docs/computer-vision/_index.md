---
title : computer-vision Tutorial
slug : computer-vision-tutorial
weight : 9984
draft : false
images : []
type : docs
---

Digital Image Processing and Computer Vision is an interesting field since it is beautifully located between in Mathematics and Computer Science. Therefore, it is very helpful to understand the fundamentals and apply them using programming to understand the topic. 

Digital images are discretization of 2 or 3 dimensional signals. In other words, digital images are sampled  sets of pixels or voxels of continuous domains.

                                            f : R² ⊃ Ω → R
   
   
   Where the f is digital image over 
   Ω : Rectangular image domain 


For the sake of simplicity, we will only discuss 2 dimensional digital images, like the ones in our StackOverflow avatars. 


About pixels: A quicknote about pixel values before we start discussing about the types of images. As a rule of thumb pixels start with value 0 which denotes no light(black), reaches to 1, maximum intensity(e.g. white), and they are represented in integers. 

**Binary Images:** Black and white only images. Each pixel is either 0 or 1, each pixel can be represented by a bit. They are not very popularly know, as they are generally used in scientific applications or for other image processing operations as mask for example. 

  [![Lena image in binary form][1]][1]

A binary image example. (Warning the image pixel values of this file is not necessarily binary, this is for demonstration, also this is Lena, the star of Image Processing world)


**Grayscale Images:** Thanks to online filters, everybody still knows these images very well. These images are generally one byte per pixel, with 0 being black and 255 being white, everything between is a different tone a gray, given that humans can only distinguish 40 shades of gray, this range is enough for many applications(Note that the values of pixels here are mapped from 0 to 1 to byte values 0 - 255)

[![Lena image in Grayscale][2]][2]


**Color Images:** Finally, most common digital image type, color images. We have to mention the concept of channels here. Digital images, also do have channels, actually, binary and grayscale images described above also have channels. Most common description would be RGB(Red-Green-Blue) model, in such case image has 3 channels(Don't confuse it with dimensions, these are still 2D images) to describe the redness, blueness, and greenness of image. In this case, each pixel is a triplet, a value between 0 - 255 (No red to most red), 0 - 255 (No green to most green), 0 - 255 (No blue to most blue). For this model, pixel {0,0,0} is black, {255,255,255} is white, {255,0,0} is red , {255, 255, 0} is yellow. However, color is a very broad topic, and you can check the references for more information.


[![Lena image in color][3]][3]



**Hyper-Spectral Images:**

After we discussed the channels, talking about hyper-spectral images is easier. These images can have hundreds of channels and commonly used in microscopy, satellite imaging etc. 

Readings

1) Signal sampling: https://en.wikipedia.org/wiki/Sampling_(signal_processing)

2) Bible of Digital Image Processing: R. C. Gonzalez, R. E. Woods: Digital Image Processing. Third Edition, Pearson
Prentice Hall, Upper Saddle River, 2008.

3) Computer Vision Review(Until Deep Learning): R. Szeliski: Computer Vision: Algorithms and Applications. Springer, New York,2010.

4) To get an understanding of binary, grayscale, color images: https://en.wikipedia.org/wiki/Grayscale


  [1]: https://i.stack.imgur.com/iy5qm.png
  [2]: https://i.stack.imgur.com/QCaSg.png
  [3]: https://i.stack.imgur.com/XMJ0G.png

