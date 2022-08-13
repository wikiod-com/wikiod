---
title: "Blob Detection"
slug: "blob-detection"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Circular Blob Detection
<!-- language-all: cpp -->

This example shows how to find circular blobs in an grayscale image. The evaluation of the circularity of a blob is done using the area and the perimeter (arc length) of the contour. The center point gets evaluated using the moments of the contour.
    
 

    #include "opencv/cv.h"
    #include "opencv/highgui.h"
    #include "opencv/cxcore.h"
    
    using namespace cv;
    
    int main(int argc, char** argv)
    {
        Mat img = imread("image.jpg", CV_LOAD_IMAGE_GRAYSCALE);
        Mat resultImg;
        cvtColor(img, resultImg, CV_GRAY2BGR);
    
        // threshold the image with gray value of 100
       Mat binImg;
       threshold(img, binImg, 100, 255, THRESH_BINARY);
    
        // find the contours
        vector<vector<Point>> contours;
        vector<Vec4i> hierarchy;
        findContours(binImg, contours, hierarchy, CV_RETR_CCOMP, CV_CHAIN_APPROX_SIMPLE);
    
        if(contours.size() <= 0)
        {
            printf("no contours found");
            return 0;
        }
        // filter the contours
        vector<vector<Point>> filteredBlobs;
        Mat centers = Mat::zeros(0,2,CV_64FC1);
        for(int i = 0; i < contours.size(); i++)
        {
            // calculate circularity
            double area = contourArea(contours[i]);
            double arclength = arcLength(contours[i], true);
            double circularity = 4 * CV_PI * area / (arclength * arclength);
            if(circularity > 0.8)
            {
                filteredBlobs.push_back(contours[i]);
            
                //calculate center
                Moments mu = moments(contours[i], false);
                Mat centerpoint = Mat(1,2,CV_64FC1);
                centerpoint.at<double>(i,0) = mu.m10 / mu.m00; // x-coordinate
                centerpoint.at<double>(i,1) = mu.m01 / mu.m00; // y-coordinate
                centers.push_back(centerpoint);
            }
        }
    
        if(filteredBlobs.size() <= 0)
        {
            printf("no circular blobs found");
            return 0;
        }
        drawContours(resultImg, filteredBlobs, -1, Scalar(0,0,255), CV_FILLED, 8);
    
        imshow("Blobs",resultImg);
        waitKey(0);
        return 0;
    }

