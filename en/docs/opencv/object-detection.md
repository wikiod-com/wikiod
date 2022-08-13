---
title: "Object Detection"
slug: "object-detection"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Template Matching with Java
**Java Source Code**
--------

    import org.opencv.core.Core;
    import org.opencv.core.Core.MinMaxLocResult;
    import org.opencv.core.Mat;
    import org.opencv.core.Point;
    import org.opencv.core.Scalar;
    import org.opencv.imgcodecs.Imgcodecs;
    import org.opencv.imgproc.Imgproc;
     
    public class TemplateMatching {
     
        public static void main(String[] args) {
            
            System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
            Mat source=null;
            Mat template=null;
            String filePath="C:\\Users\\mesutpiskin\\Desktop\\Object Detection\\Template Matching\\Sample Image\\";
            //Load image file
            source=Imgcodecs.imread(filePath+"kapadokya.jpg");
            template=Imgcodecs.imread(filePath+"balon.jpg");
        
            Mat outputImage=new Mat();    
            int machMethod=Imgproc.TM_CCOEFF;
            //Template matching method
            Imgproc.matchTemplate(source, template, outputImage, machMethod);
     
        
            MinMaxLocResult mmr = Core.minMaxLoc(outputImage);
            Point matchLoc=mmr.maxLoc;
            //Draw rectangle on result image
            Imgproc.rectangle(source, matchLoc, new Point(matchLoc.x + template.cols(),
                    matchLoc.y + template.rows()), new Scalar(255, 255, 255));
     
            Imgcodecs.imwrite(filePath+"sonuc.jpg", source);
            System.out.println("Complated.");
        }
     
    }



RESULT
------
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/JIoQ8.jpg

