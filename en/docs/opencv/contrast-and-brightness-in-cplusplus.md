---
title: "Contrast and Brightness in C++"
slug: "contrast-and-brightness-in-c++"
draft: false
images: []
weight: 9545
type: docs
toc: true
---

## Syntax

- void cv::Mat::convertTo(OutputArray m, int rtype,double alpha = 1,double beta = 0)const

## Parameters

| Parameter | Details |
|-----------|---------|
| m | output matrix; if it does not have a proper size or type before the operation, it is reallocated |
| rtype | desired output matrix type or, rather, the depth since the number of channels are the same as the input has; if rtype is negative, the output matrix will have the same type as the input |
| alpha |   optional scale factor. This changes the contrast of an image. Values below 1 decrease the contrast and above one increases the contrast |
| beta  |  optional delta added to the scaled values. Positive values increases the brightness and negative values decreases the brightnes  |

**Contrast**: 

Contrast is the difference in luminance or colour that makes an object (or its representation in an image or display) distinguishable. The higher the difference between a pixel and its neighbors the higher the contrast is in that area. 

**Brightness**: 

In other words, brightness is the perception elicited by the luminance of a visual target. 
In terms of pixels, the higher the value of a pixel is the brighter that pixel is. 

**Contrast and Brightness adjustments:** 

g(i,j)= α.f(i,j)+β
------------------

`f(x)` as the source image pixels and `g(x)` as the output image pixels.

`i` and `j` indicates that the pixel is located in the i-th row and j-th column.

The parameters `α > 0` and `β` are often called the gain and bias parameters; sometimes these parameters are said to control *contrast* and *brightness* respectively.

Opencv has a function called [convertTo()][1] which can apply these two operations. 

Sources: 
http://docs.opencv.org/trunk/d3/d63/classcv_1_1Mat.html#adf88c60c5b4980e05bb556080916978b
http://opencv-srf.blogspot.ca/2013/07/change-contrast-of-image-or-video.html 
http://opencv-srf.blogspot.ca/2013/07/change-brightness.html

  [1]: http://docs.opencv.org/trunk/d3/d63/classcv_1_1Mat.html#adf88c60c5b4980e05bb556080916978b

## Adjusting brightness and contrast of an image in c++
    // main.cpp : Defines the entry point for the console application.
    //
    #include "opencv2/highgui/highgui.hpp"
    #include <iostream>
    
    using namespace cv;
    using namespace std;
    
    int main(int argc, const char** argv)
    
    {
    
        Mat img = imread("lena30.jpg", CV_LOAD_IMAGE_COLOR); //open and read the image
    
    
        if (img.empty())
        {
            cout << "Image cannot be loaded..!!" << endl;
            return -1;
        }
    
        Mat img_higher_contrast;
        img.convertTo(img_higher_contrast, -1, 2, 0); //increase the contrast (double)
    
        Mat img_lower_contrast;
        img.convertTo(img_lower_contrast, -1, 0.5, 0); //decrease the contrast (halve)
    
        Mat img_higher_brightness;
        img.convertTo(img_higher_brightness, -1, 1, 20); //increase the brightness by 20 for each pixel 
    
        Mat img_lower_brightness;
        img.convertTo(img_lower_brightness, -1, 1, -20); //decrease the brightness by 20 for each pixel 
    
        //create windows
        namedWindow("Original Image", CV_WINDOW_AUTOSIZE);
        namedWindow("High Contrast", CV_WINDOW_AUTOSIZE);
        namedWindow("Low Contrast", CV_WINDOW_AUTOSIZE);
        namedWindow("High Brightness", CV_WINDOW_AUTOSIZE);
        namedWindow("Low Brightness", CV_WINDOW_AUTOSIZE);
        //show the image
        imshow("Original Image", img);
        imshow("High Contrast", img_higher_contrast);
        imshow("Low Contrast", img_lower_contrast);
        imshow("High Brightness", img_higher_brightness);
        imshow("Low Brightness", img_lower_brightness);
    
        waitKey(0); //wait for key press
        destroyAllWindows(); //destroy all open windows
        return 0;
    }

Output of the program:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/0uKBq.jpg

