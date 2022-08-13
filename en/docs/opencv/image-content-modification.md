---
title: "Image Content Modification"
slug: "image-content-modification"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Image color modification in OpenCV - kmeans(). To scan all the pixels of an image and replace the pixel values with generic colors.
<code>
#include opencv2/opencv.hpp>
#include <code>vector>
<code>using namespace std;
using namespace cv;
int main()
{
    Mat3b img = imread("test.jpg");

    imshow("Original", img);

    // Cluster

    int K = 8;
    int n = img.rows * img.cols;
    Mat data = img.reshape(1, n);
    data.convertTo(data, CV_32F);

    Mat labels;
    Mat1f colors;
    kmeans(data, K, labels, cv::TermCriteria(), 1, cv::KMEANS_PP_CENTERS, colors);

    for (int i = 0; i < n; ++i)
    {
        data.at<float>(i, 0) = colors(labels.at<int>(i), 0);
        data.at<float>(i, 1) = colors(labels.at<int>(i), 1);
        data.at<float>(i, 2) = colors(labels.at<int>(i), 2);
    }

    Mat reduced = data.reshape(3, img.rows);
    reduced.convertTo(reduced, CV_8U);



    imshow("Reduced", reduced);
    waitKey(0);

    return 0;
}

## Pixel by pixel modification of images
In OpenCV, images can be RGB/BGR, HSV, grayscaled, black-white and so on. It is crucial to know the data type before dealing with images.

The image data types are mainly `CV_8UC3` (Matrix of uchar with 3 channels) and CV_8U (Matrix of uchar with 1 channel), however, the conversion to other types such as CV_32FC3, CV_64F are also possible. (see [data types][1])

Consider, the image is an RGB image which is read by `imread` function.

    Mat rgb = imread('path/to/rgb/image', CV_LOAD_IMAGE_COLOR);
    //to set RED pixel value of (i,j)th to X,
    rgb.at<Vec3b>(i,j)[0] = X;

Similarly, if the image is grayscaled,

    gray.at<uchar>(i,j) = X;

Note that, in OpenCV, Black&White images are stored as CV_8U type with the values 0 and 255. Therefore, changing BW images are same as gray images. 




  [1]: https://www.wikiod.com/opencv/basic-structures#DataType

## Set Whole Image to a Solid Color
Given a non-empty `cv::Mat img` of some size, we can fill it to a solid color in several ways: 

    img = cv::Scalar(blueVal,greenVal,redVal);

or, the more general, mask supporting, `cv::Mat::setTo()`:  
    
    img.setTo(cv::Scalar(blueVal,greenVal,redVal));

If you are using the older OpenCV C API with `IplImage* img`:  

Use:

    cvSet(img, CV_RGB(redVal,greenVal,blueVal));


