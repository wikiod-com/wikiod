---
title: "Drawing Functions in Java"
slug: "drawing-functions-in-java"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Draw rectangle on image
    public class DrawRectangle {
     
        public static void main(String[] args) {
        //Load native library
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
        //image container object
        Mat goruntuDizisi=new Mat();
        //Read image in file system
        goruntuDizisi=Imgcodecs.imread("C:\\image.jpg");    
        //Draw rectangle
        //Parameters: mat object for drawing, point coordinates (x1,y1,x2,y2) and color BGR
        Imgproc.rectangle(goruntuDizisi, new Point(10,100), new Point(100,200),new Scalar(76,255,0));
        Imgcodecs.imwrite("C:\\Yeni_kiz_kulesi.jpg", goruntuDizisi);
        System.out.println("Writed");
        }
    }

