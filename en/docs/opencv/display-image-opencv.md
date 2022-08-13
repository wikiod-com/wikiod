---
title: "Display Image OpenCV"
slug: "display-image-opencv"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Basic reading and display of an image
   <!-- language: lang-py -->
    import cv2
    
    image_path= #put your image path here
    
    #use imread() function to read image data to variable img.
    img = cv2.imread(image_path) 

    #display image data in a new window with title 'I am an image display window'
    cv2.imshow('I am an image display window',img) 

    #wait until user hits any key on keyboard
    cv2.waitKey(0) 

    #close any windows opened by opencv
    cv2.destroyAllWindows() 

To control the size of the display window on the screen, add the following commands before the cv2.imshow command:
   <!-- language: lang-py -->
    window_width=800 #size of the display window on the screen
    window_height=600
   
    #open an empty window with a title.
    #The flag cv2.WINDOW_NORMAL allows the window to be scaleable.
    cv2.namedWindow('I am an image display window', cv2.WINDOW_NORMAL)

    #scale the image display window to desired size
    cv2.resizeWindow('I am an image display window', window_width, window_height)



see [openCV docs][1] for further details


  [1]: http://docs.opencv.org/3.0-beta/doc/py_tutorials/py_gui/py_image_display/py_image_display.html


## Display Image OpenCV Java
Basic reading image from java
    
    import org.opencv.core.Core;
    import org.opencv.core.Mat;
    import org.opencv.imgcodecs.Imgcodecs;
         
    //Load native library
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
    //Mat object used to host the image
    Mat imageArray;
    //Read image file from vile system
    imageArray=Imgcodecs.imread("path/to/image");


If you want to view images you can not use imshow because OpenCV-java does not have this method either. Instead, you can write the following method.

    private static BufferedImage ConvertMat2Image(Mat imgContainer{
        MatOfByte byteMatData = new MatOfByte();
        //image formatting
        Imgcodecs.imencode(".jpg", imgContainer,byteMatData);
        // Convert to array
        byte[] byteArray = byteMatData.toArray();
        BufferedImage img= null;
        try {
            InputStream in = new ByteArrayInputStream(byteArray);
            //load image
            img= = ImageIO.read(in);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
        return img;
    }
You can view the result object in the Jframe, Jlabel (jlabel icon) etc.

## Reading MJPEG from IP camera
    import cv2
    import numpy as np
    import urllib
    
    stream=urllib.urlopen('http://96.10.1.168/mjpg/video.mjpg')
    bytes=''
    while True:
        bytes+=stream.read(1024)
        a = bytes.find('\xff\xd8') # JPEG start
        b = bytes.find('\xff\xd9') # JPEG end
        if a!=-1 and b!=-1:
            jpg = bytes[a:b+2] # actual image
            bytes= bytes[b+2:] # other informations

            # decode to colored image ( another option is cv2.IMREAD_GRAYSCALE )
            img = cv2.imdecode(np.fromstring(jpg, dtype=np.uint8),cv2.IMREAD_COLOR) 
            cv2.imshow('Window name',img) # display image while receiving data
            if cv2.waitKey(1) ==27: # if user hit esc
                exit(0) # exit program


Every `JPEG` starts with `0xff 0xd8` and ends with `0xff 0xd9`. Between those are actual image. Detailed information in [this SO answer][1]


  [1]: http://stackoverflow.com/a/21844162/1804107 "this SO answer"

