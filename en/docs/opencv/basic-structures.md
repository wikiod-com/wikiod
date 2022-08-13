---
title: "Basic Structures"
slug: "basic-structures"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

This topic covers basic structures in OpenCV. The structures that will be discussed in this topic are `DataType`, `Point`, `Vec`, `Size`, `Rect`, `Scalar`, `Ptr` and `Mat`. 



## DataType
The primitive types in OpenCV are `unsigned char, bool, signed char, unsigned short, signed short, int, float, double`. 
Any data type in OpenCV is defined as `CV_<bit-depth>{U|S|F}C(<number_of_channels>)` where `U: unsigned`, `S:signed` and `F:floating point`. 

For example, `CV_32FC2` is a 32-bit, floating-point, and 2-channels structure. and the definition of basic, one channel types are

    #define CV_8U   0
    #define CV_8S   1
    #define CV_16U  2
    #define CV_16S  3
    #define CV_32S  4
    #define CV_32F  5
    #define CV_64F  6
    #define CV_USRTYPE1 7

The other types with higher channel are produced from these by the following definition:

    #define CV_MAKETYPE(depth,cn) (CV_MAT_DEPTH(depth) + (((cn)-1) << CV_CN_SHIFT))

Using these datatypes other structures can be created. 



## Mat
`Mat` (Matrix) is an n-dimensional array that can be used to store various type of data, such as RGB, HSV or grayscale images, vectors with real or complex values, other matrices etc. 

A `Mat` contains the following information: `width`, `height`, `type`, `channels`, `data`, `flags`, `datastart`, `dataend` and so on.

It has several methods, some of them are: `create`, `copyTo`, `convertTo`, `isContinious` etc.

There are many ways to create a Mat variable. Consider I want to create a matrix with 100 rows, 200 columns, type CV_32FC3:

    int R = 100, C = 200;
    Mat m1; m1.create(R,C,CV_32FC3);//creates empty matrix
    Mat m2(cv::Size(R, C), CV_32FC3); // creates a matrix with R rows, C columns with data type T where R and C are integers, 
    Mat m3(R,C,CV_32FC3); // same as m2


Initializing Mat:

    Mat m1 = Mat::zeros(R,C,CV_32FC3); // This initialized to zeros, you can use one, eye or cv::randn etc.
    Mat m2(R,C,CV_32FC3);
    for (int i = 0; i < m2.rows; i++)
        for (int j = 0; j < m2.cols; j++)
            for (int k = 0; k < m2.channels(); k++)
                m2.at<Vec3f>(i,j)[k] = 0;
    //Note that, because m2 is a float type and has 3 channels, we used Vec3f, for more info see Vec 

    Mat m3(3, out, CV_32FC1, cv::Scalar(0));





## Vec
`Vec` (Vector) is a template class for numerical values. Unlike `c++ vector`s, it generally stores short vectors (only a few elements).

The way a `Vec` is defined is as follows:

     typedef Vec<type, channels> Vec< channels>< one char for the type>;

where type is one of `uchar, short, int, float, double` and the characters for each type are `b, s, i, f, d`, respectively.

For example, `Vec3b` indicates an unsigned char vector of 3 channels. Each index in an RGB image is in this format. 

    Mat rgb = imread('path/to/file', CV_LOAD_IMAGE_COLOR);  
    cout << rgb.at<Vec3b>(0,0); //The output is [r g b] values as ASCII character.
    // To print integer values of RED value
    cout << (int)rgb.at<Vec3b>(0,0)[0]; //The output will be an integer in [0, 255].

In `Vec` class the following operators are defined

    v1 = v2 + v3
    v1 = v2 - v3
    v1 = v2 * scale
    v1 = scale * v2
    v1 = -v2
    v1 += v2 and other augmenting operations
    v1 == v2, v1 != v2

For more information, see the [link][1]


  [1]: http://docs.opencv.org/2.4.10/modules/core/doc/basic_structures.html#vec

