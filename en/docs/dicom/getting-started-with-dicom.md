---
title: "Getting started with dicom"
slug: "getting-started-with-dicom"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Understanding the DICOM File Format
The DICOM Image file is a tagged image file; the file contains both an image (most of the time) and a collection of data about the image. The data in a DICOM image file is stored as a sequence of individual elements. Each element contains one item of information about the image or the image itself. DICOM elements are binary, so DICOM files cannot be viewed with a text editor.

DICOM elements have several components. These are;

    tag - a number which identifies the type of element
    data type - a description of the data type of the data in the element
    length - the number of bytes of data in the 
    data - the data stored in the element

an example;

    0010,0010 PN 12 Elemans^John

In this example the tag is broken into two parts, the group and element numbers. Group and element numbers will be explained elsewhere. It is important to note that the above example is an ASCII representation of the example element. In hex it appears as follows;

    10001000 504E0C00 454C454D 414E535E 4A4F484E

Note the byte order in the tag and element length, it can be either in an actual file. So the parts are;

    tag - 10001000 = 00100010 or 0010,0010
    type - 504E = PN
    length - 0C00 = 12
    data - 54C454D 414E535E 4A4F484E = Elemans^John

All elements in a DICOM file are stored in ascending sequence of tag numbers.

Note that the data type PN is not just a string type. DICOM specifies types which are more complex than simple programming types. PN defines the layout of the string in order to indicate the name parts etc. 

As mentioned, the image data itself is just another element in a DICOM file. The image data element is the last element in a file and looks like this;

    tag - 7FE0,0010
    type - OB or OW (other Byte or other Word)
    length - depends on the image
    data - binary data for the image

Because DICOM allows a wide variety of image data formats, one cannot simply read the last tag and display it. Other elements describe the image size, bits per pixel, colour data etc.


## Installation or Setup
Detailed instructions on getting dicom set up or installed.

