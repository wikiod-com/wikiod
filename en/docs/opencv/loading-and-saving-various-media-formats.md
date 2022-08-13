---
title: "Loading and Saving Various Media Formats"
slug: "loading-and-saving-various-media-formats"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Loading Videos
Show how to use `cv::VideoCapture`. Here is the example of loading video from file:

    #include "opencv2/highgui/highgui.hpp"
    #include "opencv2/imgproc/imgproc.hpp"
    #include "opencv2/core/core.hpp"
    #include <iostream>

    using namespace cv;
    VideoCapture videoSource;
    Mat frame;
    #define VIDEO_PATH "video.avi"

    int main() 
    {
        //Open video
        if (!videoSource.open(VIDEO_PATH))
        {            
            std::cout<<"Video not found at "<<VIDEO_PATH<<std::endl;
            return 1;     // Exit if fail
        }
        videoSource.set(CV_CAP_PROP_CONVERT_RGB, 1);
        
        int cameraWidth = videoSource.get(CV_CAP_PROP_FRAME_WIDTH);
        int cameraHeight = videoSource.get(CV_CAP_PROP_FRAME_HEIGHT);
        float cameraAspectRatio = cameraWidth / cameraHeight;

        std::cout <<"Camera resolution: " << cameraWidth<<", "<<cameraHeight<<" aspect ratio: "<<cameraAspectRatio<< std::endl;
        
        while(true)
        {
            videoSource >> frame;
            if(frame.empty())
                break;  
            //Resize frame
            cv::resize(frame, frame, cv::Size(320, 320 / cameraAspectRatio));
            imshow("frame", frame);
            waitKey(20);
        }
       waitKey(0);
       return 0;
    }

## Live Capture
Show how to use `cv::VideoCapture` with e.g. a webcam. Capturing frames from webcam and display it. Here is the example code:

    #include <iostream> 

    #include "opencv2/highgui/highgui.hpp"
    #include "opencv2/imgproc/imgproc.hpp"
    #include "opencv2/core/core.hpp"
   
    using namespace cv;
    VideoCapture videoSource;
    Mat frame;

    int main() 
    {
        if(!videoSource.open(0)) //if more cameras available use 1,2,...
            return 1;
     
        while(true)
        {            
            videoSource >> frame;
            if(frame.empty())
                break;
            imshow("Webcam", frame); //or any kinf of precessing
            if(waitKey(1)==27)
                break;//stop capturing is ESC pressed
        }
    
       return 0;
    }

## Saving Videos
Show how to use cv::VideoWriter.

    #include "opencv2/highgui/highgui.hpp"
    #include <iostream>

    using namespace cv;
    using namespace std;

    int main(int argc, char* argv[])
    {
        VideoCapture cap(0); // open the video camera no. 0
    
        if (!cap.isOpened())  // if not success, exit program
        {
            cout << "ERROR: Cannot open the video file" << endl;
            return -1;
        }

        namedWindow("MyVideo",CV_WINDOW_AUTOSIZE); //create a window called "MyVideo"
     
        double dWidth = cap.get(CV_CAP_PROP_FRAME_WIDTH); //get the width of frames of the video
        double dHeight = cap.get(CV_CAP_PROP_FRAME_HEIGHT); //get the height of frames of the video
    
        cout << "Frame Size = " << dWidth << "x" << dHeight << endl;

        Size frameSize(static_cast<int>(dWidth), static_cast<int>(dHeight));

        VideoWriter oVideoWriter ("D:/MyVideo.avi", CV_FOURCC('P','I','M','1'), 20, frameSize, true); //initialize the VideoWriter object 

        if ( !oVideoWriter.isOpened() ) //if not initialize the VideoWriter successfully, exit the program
       {
            cout << "ERROR: Failed to write the video" << endl;
            return -1;
       }

       while (1)
       {
  
        Mat frame;

        bool bSuccess = cap.read(frame); // read a new frame from video

        if (!bSuccess) //if not success, break loop
        {
             cout << "ERROR: Cannot read a frame from video file" << endl;
             break;
        }

        oVideoWriter.write(frame); //writer the frame into the file

        imshow("MyVideo", frame); //show the frame in "MyVideo" window

        if (waitKey(10) == 27) //wait for 'esc' key press for 30ms. If 'esc' key is pressed, break loop
       {
            cout << "esc key is pressed by user" << endl;
            break; 
       }
    }

    return 0;

}



## Saving Images
Actually, Live Capture example is good for capturing images, so I am using it to capture images and save them in a folder. 

    #include <fstream>
    #include <string>
    
    #include <opencv2/highgui/highgui.hpp>
    #include <opencv2/core/core.hpp>
    #include <opencv2/imgproc/imgproc.hpp>
    
    
    int main() 
    {
        std::stringstream file; // to write the file name 
        
        cv::VideoCapture cap(0); // create a capture object
    
        int counter = 0; // Create counter
        
        while(true) // infinite loop
        { 
            cv::Mat frame; // Create a object 
           
            cap.read(frame); // read the frame
    
            file << "/home/user/path_to_your_folder/image" << counter << ".jpg"; // file name
    
            cv::imwrite(file.str(), frame);
            
            counter++; // increment the counter
        }
    
       return 0;
    }

## Loading Images
```
#include <highgui.h>

//...

cv::Mat img = cv::imread("img.jpg");
```
...

