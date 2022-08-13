---
title: "BOOST- Compare Images using OpevCV"
slug: "boost--compare-images-using-opevcv"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This documentation explains how an external Image can be tested and compared with the output image of OpenCV. For example, To compare two blurred images and test if they both are same, we blur an original image in an external software (I used WiT Image Processing software) or just download any blurred image online- output1. Create a Win32 OpenCV project in Visual Studio. Read the original image as an input to the OpenCV. Blur the original image in OpenCV and compare with output1.

## OpenCV code to read Images and compare
#include <opencv2/opencv.hpp>
#include <iostream>

using namespace cv;
using namespace std;

int main(int argc, char** argv)
{
    Mat image;
    image = imread("C:\\Users\\Development\\Documents\\Visual Studio 2013\\Projects\\ImageIn.bmp", CV_LOAD_IMAGE_GRAYSCALE);   // Read the file

    if (!image.data)                              // Check for invalid input
    {
        cout << "Could not open or find the image" << std::endl;
        return -1;
    }

    

    Mat witout = imread("C:\\Users\\Development\\Documents\\Visual Studio 2013\\Projects\\ImageWitOut.bmp", CV_LOAD_IMAGE_GRAYSCALE);;
    Mat cvout = Mat(image.size(), image.type(), Scalar(255));

    imshow("witout", witout);
    imshow("cvout", cvout);

    Mat diff = (witout == cvout);

    namedWindow("Difference", WINDOW_AUTOSIZE);// Create a window for display.
    imshow("Difference", diff);                   // Show our image inside it.

    waitKey(0);                                          // Wait for a keystroke in the window
    return 0;
}

