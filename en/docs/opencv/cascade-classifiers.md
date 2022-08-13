---
title: "Cascade Classifiers"
slug: "cascade-classifiers"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Using Cascade Classifiers to detect face

# Python

## Code

    import numpy as np
    import cv2

    #loading haarcascade classifiers for face and eye
    #You can find these cascade classifiers here
    #https://github.com/opencv/opencv/tree/master/data/haarcascades
    #or where you download opencv inside data/haarcascades

    face_cascade = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')
    eye_cascade = cv2.CascadeClassifier('haarcascade_eye.xml')

    #loading the image
    img = cv2.imread('civil_war.jpg')

    #converting the image to gray scale
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

    #detecting face in the grayscale image
    faces = face_cascade.detectMultiScale(gray, 1.3, 5)

    #iterate through each detected face
    for (x,y,w,h) in faces:
        cv2.rectangle(img,(x,y),(x+w,y+h),(255,0,0),2) #draw rectangle to each detected face

        #take the roi of the face (region of interest) 
        roi_gray = gray[y:y+h, x:x+w]
        roi_color = img[y:y+h, x:x+w]

        #detect the eyes
        eyes = eye_cascade.detectMultiScale(roi_gray)
        for (ex,ey,ew,eh) in eyes:

            #draw rectangle for each eye
            cv2.rectangle(roi_color,(ex,ey),(ex+ew,ey+eh),(0,255,0),2)

    #show the image
    cv2.imshow('img',img)
    cv2.waitKey(0)
    cv2.destroyAllWindows()
    
## Result

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/T7Jnv.png

## Face detection using haar cascade classifier
**C++**
---

    #include "opencv2/objdetect/objdetect.hpp"
    #include "opencv2/highgui/highgui.hpp"
    #include "opencv2/imgproc/imgproc.hpp"
    
    #include <iostream>
    #include <stdio.h>
    
    using namespace std;
    using namespace cv;
    
    // Function Headers
    void detectAndDisplay(Mat frame);
    
    // Global variables
    string face_cascade_name = "./data/haarcascade_frontalface_alt2.xml";
    CascadeClassifier face_cascade;
    
    // Function main
    int main(void)
    {
        // Load the cascade
        if (!face_cascade.load(face_cascade_name)){
            printf("--(!)Error on cascade loading\n");
            return (-1);
        }
    
        // Read the image file
        Mat frame = imread("d:/obama_01.jpg");
    
        // Apply the classifier to the frame
        if (!frame.empty())
            detectAndDisplay(frame);
        waitKey(0);
        return 0;
    }
    
    // Function detectAndDisplay
    void detectAndDisplay(Mat frame)
    {
        std::vector<Rect> faces;
        Mat frame_gray;
        
        cvtColor(frame, frame_gray, COLOR_BGR2GRAY);
        equalizeHist(frame_gray, frame_gray);
    
        // Detect faces
        face_cascade.detectMultiScale(frame_gray, faces, 1.1, 2, 0 | CASCADE_SCALE_IMAGE, Size(30, 30));
    
        for (int ic = 0; ic < faces.size(); ic++) // Iterate through all current elements (detected faces)
        {
            Point pt1(faces[ic].x, faces[ic].y); // Display detected faces on main window - live stream from camera
            Point pt2((faces[ic].x + faces[ic].height), (faces[ic].y + faces[ic].width));
            rectangle(frame, pt1, pt2, Scalar(0, 255, 0), 2, 8, 0);
        }
    
        imshow("original", frame);
    }

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/7uBQn.jpg

## Cascade Classifiers to detect face with Java
**Java**
--------
Code




    import org.opencv.core.Mat;
    import org.opencv.core.MatOfRect;
    import org.opencv.core.Point;
    import org.opencv.core.Rect;
    import org.opencv.core.Scalar;
    import org.opencv.highgui.Highgui;
    import org.opencv.highgui.VideoCapture;
    import org.opencv.objdetect.CascadeClassifier;

    public class FaceDetector{
     
        public static void main(String[] args) {
     
            System.loadLibrary(Core.NATIVE_LIBRARY_NAME); 
            //Create object
            CascadeClassifier faceDetector = new CascadeClassifier(FaceDetector.class.getResource("haarcascade_frontalface_default.xml").getPath());
           
            //Read image
            Mat image = Highgui.imread("sourceimage.jpg");
           
         /*        
            //Or read from webcam
           
             * Mat image=new Mat();
             *VideoCapture videoCapture=new VideoCapture(0);
             *videoCapture.read(image);
         */
            MatOfRect faceDetections = new MatOfRect();
            //Result list
            faceDetector.detectMultiScale(image, faceDetections);

            for (Rect rect : faceDetections.toArray()) {
                //Draw rectangle on result
         
                Core.rectangle(image, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height),
                        new Scalar(0, 255, 0));
            }
            
            //write result
            Highgui.imwrite("result.png", image);
            System.out.println("Succesfull");
        }
        
        
    }
Result

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/aH5R6.png

