---
title: "Edge detection"
slug: "edge-detection"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

## Syntax
 - edges = cv2.[Canny][1](image, threshold1, threshold2[, edges[, apertureSize[, L2gradient]]])
 - void [Canny][1](InputArray image, OutputArray edges, double threshold1, double threshold2, int apertureSize=3, bool L2gradient=false


  [1]: http://docs.opencv.org/2.4.13/modules/imgproc/doc/feature_detection.html#canny

## Parameters
| Parameter | Details |
|-----------|---------|
|image|Input image|
|edges|Output image|
|threshold1|First threshold for hysteresis procedure|
|threshold2|Second threshold for hysteresis procedure|
|apertureSize|Aperture size for Sobel operator|
|L2gradient|Flag indicating whether a more accurate algorithm for image gradient should be used|

## Canny algorithm
The Canny algorithm is a more recent edge detector designed as a signal processing problem. In OpenCV, it outputs a binary image marking the detected edges. 

**Python:**

    import cv2
    import sys
    
    # Load the image file
    image = cv2.imread('image.png')
    
    # Check if image was loaded improperly and exit if so
    if image is None:
        sys.exit('Failed to load image')
    
    # Detect edges in the image. The parameters control the thresholds
    edges = cv2.Canny(image, 100, 2500, apertureSize=5)
    
    # Display the output in a window
    cv2.imshow('output', edges)
    cv2.waitKey()

## Canny Algorithm - C++
Below is an usage of canny algorithm in c++. Note that the image is first converted to grayscale image, then Gaussian filter is used to reduce the noise in the image. Then Canny algorithm is used for edge detection.

 
    // CannyTutorial.cpp : Defines the entry point for the console application. 
    // Environment: Visual studio 2015, Windows 10
    // Assumptions: Opecv is installed configured in the visual studio project
    // Opencv version: OpenCV 3.1
    
    #include "stdafx.h"
    #include<opencv2/highgui/highgui.hpp>
    #include<opencv2/imgproc/imgproc.hpp>
    #include<string>
    #include<iostream>
    
    
    
    int main()
    {
    
        //Modified from source: https://github.com/MicrocontrollersAndMore/OpenCV_3_Windows_10_Installation_Tutorial
        cv::Mat imgOriginal;        // input image
        cv::Mat imgGrayscale;        // grayscale of input image
        cv::Mat imgBlurred;            // intermediate blured image
        cv::Mat imgCanny;            // Canny edge image
    
        std::cout << "Please enter an image filename : ";
        std::string img_addr;
        std::cin >> img_addr;
    
        std::cout << "Searching for " + img_addr << std::endl;
    
        imgOriginal = cv::imread(img_addr);            // open image
    
        if (imgOriginal.empty()) {                                    // if unable to open image
            std::cout << "error: image not read from file\n\n";        // show error message on command line
            return(0);                                                // and exit program
        }
    
        cv::cvtColor(imgOriginal, imgGrayscale, CV_BGR2GRAY);        // convert to grayscale
    
        cv::GaussianBlur(imgGrayscale,            // input image
            imgBlurred,                            // output image
            cv::Size(5, 5),                        // smoothing window width and height in pixels
            1.5);                                // sigma value, determines how much the image will be blurred
    
        cv::Canny(imgBlurred,            // input image
            imgCanny,                    // output image
            100,                        // low threshold
            200);                        // high threshold
    
    
        // Declare windows
        // Note: you can use CV_WINDOW_NORMAL which allows resizing the window
        // or CV_WINDOW_AUTOSIZE for a fixed size window matching the resolution of the image
        // CV_WINDOW_AUTOSIZE is the default
        cv::namedWindow("imgOriginal", CV_WINDOW_AUTOSIZE);        
        cv::namedWindow("imgCanny", CV_WINDOW_AUTOSIZE);
    
        //Show windows
        cv::imshow("imgOriginal", imgOriginal);        
        cv::imshow("imgCanny", imgCanny);
    
        cv::waitKey(0);                    // hold windows open until user presses a key
        return 0;
    }



## Calculating Canny Thresholds
http://stackoverflow.com/questions/4292249/automatic-calculation-of-low-and-high-thresholds-for-the-canny-operation-in-open

## Canny Edge Video from Webcam Capture - Python
    import cv2


    def canny_webcam():
        "Live capture frames from webcam and show the canny edge image of the captured frames."

        cap = cv2.VideoCapture(0)
    
        while True:
            ret, frame = cap.read()  # ret gets a boolean value. True if reading is successful (I think). frame is an
            # uint8 numpy.ndarray
    
            frame = cv2.GaussianBlur(frame, (7, 7), 1.41)
            frame = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    
            edge = cv2.Canny(frame, 25, 75)
    
            cv2.imshow('Canny Edge', edge)
    
            if cv2.waitKey(20) == ord('q'):  # Introduce 20 milisecond delay. press q to exit.
                break
    
    canny_webcam()

## Canny Edge Thresholds prototyping using Trackbars
    """ 
    CannyTrackbar function allows for a better understanding of 
    the mechanisms behind Canny Edge detection algorithm and rapid
    prototyping. The example includes basic use case.

    2 of the trackbars allow for tuning of the Canny function and
    the other 2 help with understanding how basic filtering affects it.
    """
    import cv2
    
    def empty_function(*args):
        pass
    
    def CannyTrackbar(img):
        win_name = "CannyTrackbars"
    
        cv2.namedWindow(win_name)
        cv2.resizeWindow(win_name, 500,100)

        cv2.createTrackbar("canny_th1", win_name, 0, 255, empty_function)
        cv2.createTrackbar("canny_th2", win_name, 0, 255, empty_function)
        cv2.createTrackbar("blur_size", win_name, 0, 255, empty_function)
        cv2.createTrackbar("blur_amp", win_name, 0, 255, empty_function)
    
        while True:
            cth1_pos = cv2.getTrackbarPos("canny_th1", win_name)
            cth2_pos = cv2.getTrackbarPos("canny_th2", win_name)
            bsize_pos = cv2.getTrackbarPos("blur_size", win_name)
            bamp_pos = cv2.getTrackbarPos("blur_amp", win_name)

            img_blurred = cv2.GaussianBlur(img.copy(), (trackbar_pos3 * 2 + 1, trackbar_pos3 * 2 + 1), bamp_pos)
            canny = cv2.Canny(img_blurred, cth1_pos, cth2_pos)
            cv2.imshow(win_name, canny)
    
            key = cv2.waitKey(1) & 0xFF
            if key == ord("c"):
                break
    
        cv2.destroyAllWindows()
        return canny
    
    img = cv2.imread("image.jpg")
    canny = CannyTrackbar(img)
    cv2.imwrite("result.jpg", canny)

