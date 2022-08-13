---
title: "Creating a Video"
slug: "creating-a-video"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

Whenever you work with video feeds you may eventually want to save your image processing result in a form of a new video file. For simple video outputs you can use the OpenCV built-in VideoWriter class, designed for this. It is useful to look at some concepts before using them. These concepts are codec ie decoder and FourCC (Four character code).

## Creating a video with OpenCV (Java)
    VideoWriter videoWriter;
    videoWriter = new VideoWriter(outputFile, VideoWriter.fourcc('x', '2','6','4'),
                    fps, frameSize, isRGB);
    //We have stated that we will use x264 as codec with FourCC
    //For writing, we add the following method and it will write the image we give as parameter in this call.
     public void Write(Mat frame) {
            if(videoWriter.isOpened()==false){
                videoWriter.release();
                throw new IllegalArgumentException("Video Writer Exception: VideoWriter not opened,"
                        + "check parameters.");        
            }
            //Write video
            videoWriter.write(frame);
        }
    
    //With Video Capture for example, we can read images from the camera and write the same video
    
    VideoCapture videoCapture = new VideoCapture(0);
    Size frameSize = new Size((int) videoCapture.get(Videoio.CAP_PROP_FRAME_WIDTH), (int) videoCapture.get(Videoio.CAP_PROP_FRAME_HEIGHT));
    VideoWriter videoWriter = new VideoWriter("test.avi", VideoWriter.fourcc('x', '2','6','4'),
                    videoCapture.get(Videoio.CAP_PROP_FPS), frameSize, true);
    while (videoCapture.read(mat)) {
                videoWriter.write(mat);         
            }
            videoCapture.release();
            videoWriter.release();

