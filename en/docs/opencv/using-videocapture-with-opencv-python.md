---
title: "Using VideoCapture With OpenCV Python"
slug: "using-videocapture-with-opencv-python"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Reading frames from a pre-captured video
[![video][1]][1] [![projection][2]][2]
    
    import numpy as np
    import cv2
    
    #access a video from your disk
    #to use the GIF in this example, convert to avi!
    cap = cv2.VideoCapture('eg_videoRead.avi')
    
    
    #we are going to read 10 frames 
    #we store the frames in a numpy structure
    #then we'll generate a minimum projection of those frames
    
    frameStack=[]
    numFrames=10
    
    for fr in range(numFrames):
        cap.set(cv2.CAP_PROP_POS_FRAMES,fr) #specifies which frame to read next
        frame=cap.read() #read the frame
        #gray = cv2.cvtColor(frame[1], cv2.COLOR_BGR2GRAY) #convert to gray scale
        frameStack.append(frame[1]) #add current frame to our frame Stack
        
    minProjection=np.min(frameStack,axis=0) #find the minimum across frames
    cv2.imshow("projection", minProjection) #show the result


  [1]: http://i.stack.imgur.com/FcdlL.gif
  [2]: http://i.stack.imgur.com/1zfVP.png

## Using VideoCapture With OpenCV Java
  There is no imshow in the java, you need to write a method for this. This method is a Mat2bufferedImage. Takes mat object as parameter and returns image.

    public static void main(String[] args) {
        Mat frame = new Mat();
        //0; default video device id
        VideoCapture camera = new VideoCapture(0);
        JFrame jframe = new JFrame("Title");
        jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        JLabel vidpanel = new JLabel();
        jframe.setContentPane(vidpanel);
        jframe.setVisible(true);
    
        while (true) {
            if (camera.read(frame)) {
    
                ImageIcon image = new ImageIcon(Mat2bufferedImage(frame));
                vidpanel.setIcon(image);
                vidpanel.repaint();
    
            }
        }
    }

