---
title: "Drawing Shapes (Line, Circle, ..., etc) in C++"
slug: "drawing-shapes-line-circle--etc-in-c++"
draft: false
images: []
weight: 9938
type: docs
toc: true
---


In OpenCV, one can draw numerous shapes such as point, line, circle, ..., etc. There is an optional for filling a shape. The following code is self-explanatory which shows how shapes are drawn.


## Drawing Shapes Sample
    #include <opencv2/core/core.hpp>
    #include <opencv2/highgui/highgui.hpp>
    #include <opencv2/imgproc.hpp> // drawing shapes
    #include <iostream>
    
    
    int main( int argc, char** argv )
    {
        // First create a black image. 
        cv::Mat image(500,500, CV_8UC3, cv::Scalar(0,0,0));
    
        // Check if the image is created successfully. 
        if( !image.data ){
            std::cout <<  "Could not open or find the image" << std::endl ;
            exit(EXIT_FAILURE);
        }
        
        
        //####################(  Draw Line  )##########################
        cv::Point p1(100,100), p2(200,100);
        cv::Scalar colorLine(0,255,0); // Green
        int thicknessLine = 2;
        
        cv::line(image, p1, p2, colorLine, thicknessLine);
        
        //####################(  Draw Circle  )#########################
        // unfilled circle
        cv::Point centerCircle1(250,250);
        int radiusCircle = 30;
        cv::Scalar colorCircle1(0,0,255);
        int thicknessCircle1 = 2;
        
        cv::circle(image, centerCircle1, radiusCircle, colorCircle1, thicknessCircle1);
        
        // filled circle
        cv::Point centerCircle2(400,100);
        cv::Scalar colorCircle2(0,100,0);
        
        cv::circle(image, centerCircle2, radiusCircle, colorCircle2, CV_FILLED);
        
        //####################(  Draw Rectangle  )#######################
        // unfilled 
        cv::Point p3(400,400), p4(450,450);
        cv::Scalar colorRectangle1(0,0,255);
        int thicknessRectangle1 = 3;
        
        cv::rectangle(image, p3, p4, colorRectangle1,thicknessRectangle1);
        
        //   filled
        cv::Point p5(100,400), p6(150,450);
        cv::Scalar colorRectangle2(255,0,255);
         
        cv::rectangle(image, p5, p6, colorRectangle2, CV_FILLED);
        
        
        //#################( Draw Shapes on Image )######################
        cv::namedWindow( "Display window", cv::WINDOW_AUTOSIZE );
        cv::imshow( "Display window", image );                  
    
        cv::waitKey(0);   
        
        
        return 0;
    }

The output is 

[![enter image description here][1]][1]

OpenCV 3.2 Mac with g++ compiler

    g++ main2.cpp -o main `pkg-config --cflags --libs opencv`



  [1]: https://i.stack.imgur.com/CPoaY.png

