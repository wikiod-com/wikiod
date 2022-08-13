---
title: "Image Processing"
slug: "image-processing"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Syntax
 1. **Gaussian Blur Syntax C++:** void GaussianBlur(InputArray src, OutputArray dst, Size ksize, double sigmaX, double sigmaY=0, int borderType=BORDER_DEFAULT )



## Parameters

| Parameters of Gaussian Blur | Details |
| ------ | ------ |
| src   | Input image, the image can have any number of channels, which are processed independently, but the depth should be `CV_8U`, `CV_16U`, `CV_16S`, `CV_32F` or `CV_64F`. |
| dst | Output image of the same size and type as `src` | 
| ksize | Gaussian kernel size. `ksize.width` and `ksize.height` can differ but they both must be **positive and odd**. Or, they can be zero’s and then they are computed from sigma* . |
| sigmaX | Gaussian kernel standard deviation in **X direction**. |
| sigmaY | Gaussian kernel standard deviation in **Y direction**. if `sigmaY` is zero, it is set to be equal to `sigmaX`, if both sigmas are zeros, they are computed from `ksize.width` and `ksize.height`. To fully control the result regardless of possible future modifications of all this semantics, it is recommended to specify all of `ksize`, `sigmaX`, and `sigmaY`. |
| borderType | Pixel extrapolation method. |

I don't think it makes sense to put syntax and parameters specific to gaussian blur in this place as the topic is so broad that it should include many other examples

## Smoothing Images with Gaussian Blur in C++
Smoothing, also known as **blurring**, is one of the most commonly used operation in Image Processing.

The most common use of the smoothing operation is to **reduce noise** in the image for further processing.

There are many algorithms to perform smoothing operation.

We'll look at one of the most commonly used filter for blurring an image, the **Gaussian Filter** using the OpenCV library function `GaussianBlur()`. This filter is designed specifically for removing *high-frequency noise* from images.

<!-- language: lang-cpp -->
    #include <opencv2/opencv.hpp>
    #include <iostream>
    
    using namespace std;
    using namespace cv;
    
    int main(int argc, char** argv){
    
        Mat image , blurredImage;
    
        // Load the image file
        image = imread(argv[1], CV_LOAD_IMAGE_COLOR);
    
        // Report error if image could not be loaded
        if(!image.data){
            cout<<"Error loading image" << "\n";
            return -1;
        }
    
        // Apply the Gaussian Blur filter. 
        // The Size object determines the size of the filter (the "range" of the blur)
        GaussianBlur( image, blurredImage, Size( 9, 9 ), 1.0);
    
        // Show the blurred image in a named window
        imshow("Blurred Image" , blurredImage);
    
        // Wait indefinitely untill the user presses a key
        waitKey(0);
    
        return 0;
    }

For the detailed mathematical definition and other types of filters you can check the  [original documentation][1].


  [1]: http://docs.opencv.org/2.4/doc/tutorials/imgproc/gausian_median_blur_bilateral_filter/gausian_median_blur_bilateral_filter.html

## Bilateral Filtering


## Thresholding
**In Python:**

[![before threshold][1]][1]

    import cv2
    image_path= 'd:/contour.png'
    img = cv2.imread(image_path)
    
    #display image before thresholding
    cv2.imshow('I am an image display window',img)
    cv2.waitKey(0)
    
    #convert image to gray scale - needed for thresholding
    img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    
    
    #apply threshold to gray image to obtain binary image
    
    threshold=150 #value above which pixel values will be set to max_value
    max_value=255  #value to which pixels above threshold will be set
    threshold_stype=cv2.THRESH_BINARY #default threshold method
    
    ret, img_binary = cv2.threshold(img_gray, threshold, max_value, threshold_stype)
    
    #display image after thresholding
    cv2.imshow('image after applying threshold',img_binary)
    cv2.waitKey(0)
    
    #save the binary image
    cv2.imwrite('d:/binary.png',img_binary)
    cv2.destroyAllWindows()
[![after threshold][2]][2]


  [1]: http://i.stack.imgur.com/7nhQ0.png
  [2]: http://i.stack.imgur.com/3NaHF.png

