---
title: "Integrate OpenCV into Android Studio"
slug: "integrate-opencv-into-android-studio"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

The Open CV libraries can be found on the web by using a search engine.


The **Gotchas**:

 - If you lower your target platform below KitKat some of the OpenCV libraries will no longer function, specifically the classes related to *org.opencv.android.Camera2Renderer* and other related classes.  You can probably get around this by simply removing the apprpriate OpenCV .java files.
 - If you raise your target platform to Lollipop or above my example of loading a file might not work because use of absolute file paths is frowned upon.  So you might have to change the example to load a file from the gallery or somewhere else.  There are numerous examples floating around.


## Instructions
Tested with A.S. v1.4.1 but should work with newer versions too.

1.  Create a new Android Studio project using the project wizard (Menu:/File/New Project):
    - Call it "**cvtest1**"
    - Form factor:  **API 19, Android 4.4 (KitKat)**
    - **Blank Activity** named **MainActivity**

    You should have a *cvtest1* directory where this project is stored.  (the title bar of Android studio shows you where cvtest1 is when you open the project)



2.  Verify that your app runs correctly.  Try changing something like the "Hello World" text to confirm that the build/test cycle is OK for you.  (I'm testing with an emulator of an API 19 device).

3.  Download the OpenCV package for Android v3.1.0 and unzip it in some temporary directory somewhere.  (Make sure it is the package specifically for Android and not just the OpenCV for Java package.)  I'll call this directory "**unzip-dir**"  Below **unzip-dir** you should have a **sdk/native/libs** directory with subdirectories that start with things like **arm**..., **mips**... and **x86**...  (one for each type of "architecture" Android runs on)

4.  From Android Studio import OpenCV into your project as a module:
    **Menu:/File/New/Import_Module**:
       - Source-directory: **{unzip-dir}/sdk/java**
       - Module name:  Android studio automatically fills in this field with **openCVLibrary310** (the exact name probably doesn't matter but we'll go with this).
       - Click on **next**.  You get a screen with three checkboxes and questions about jars, libraries and import options.  All three should be checked.  Click on **Finish.**
    
     Android Studio starts to import the module and you are shown an **import-summary.txt** file that has a list of what was not imported (mostly javadoc files) and other pieces of information.    
[![enter image description here][1]][1]

     But you also get an error message saying **failed to find target with hash string 'android-14'...**.  This happens because the build.gradle file in the OpenCV zip file you downloaded says to compile using android API version 14,
        which by default you don't have with Android Studio v1.4.1.
[![enter image description here][2]][2]


5.  Open the project structure dialogue (**Menu:/File/Project_Structure**).  Select the "app" module, click on the **Dependencies** tab and add **:openCVLibrary310** as a Module Dependency.  When you select **Add/Module_Dependency** it should appear in the list of modules you can add.
It will now show up as a dependency but you will get a few more **cannot-find-android-14** errors in the event log.

6. Look in the **build.gradle** file for your app module.  There are multiple build.gradle files in an Android project.  The one you want is in the **cvtest1/app** directory and from the project view it looks like **build.gradle (Module: app)**.  Note the values of these four fields:
    - compileSDKVersion  (mine says 23)
    - buildToolsVersion (mine says 23.0.2)
    - minSdkVersion (mine says 19)
    - targetSdkVersion (mine says 23)

7.  Your project now has a **cvtest1/OpenCVLibrary310** directory but it is not visible from the project view:
 
[![enter image description here][3]][3]

Use some other tool, such as any file manager, and go to this directory.  You can also switch the project view from **Android** to **Project Files** and you can find this directory as shown in this screenshot:
[![enter image description here][4]][4]

Inside there is another **build.gradle** file (it's highlighted in the above screenshot).  Update this file with the four values from step 6.

8.  Resynch your project and then clean/rebuild it.  (**Menu:/Build/Clean_Project**)    It should clean and build without errors 
    and you should see many references to **:openCVLibrary310** in the **0:Messages** screen.

    [![enter image description here][5]][5]

    At this point the module should appear in the project hierarchy as **openCVLibrary310**, just like **app**.  (Note that in that little drop-down menu I switched back from **Project View** to **Android View** ).  You should also see an additional **build.gradle** file under "Gradle Scripts" but I find the Android Studio interface a little bit glitchy and sometimes it does not do this right away.  So try resynching, cleaning, even restarting Android Studio.

    You should see the openCVLibrary310 module with all the OpenCV functions under java like in this screenshot:

    [![enter image description here][6]][6]

9.  Copy the **{unzip-dir}/sdk/native/libs** directory (and everything under it) to your Android project, to **cvtest1/OpenCVLibrary310/src/main/**, and then rename your copy 
from **libs** to **jniLibs**.  You should now have a **cvtest1/OpenCVLibrary310/src/main/jniLibs** directory.  Resynch your project and this directory should now
appear in the project view under **openCVLibrary310**.

    [![enter image description here][8]][8]

10.  Go to the onCreate method of *MainActivity.java* and append this code:

   
    if (!OpenCVLoader.initDebug()) {
            Log.e(this.getClass().getSimpleName(), "  OpenCVLoader.initDebug(), not working.");
        } else {
            Log.d(this.getClass().getSimpleName(), "  OpenCVLoader.initDebug(), working.");
        }

   Then run your application.  You should see lines like this in the Android Monitor:

[![enter image description here][9]][9]

 (I don't know why that line with the error message is there)

11.  Now try to actually use some openCV code.  In the example below I copied a .jpg file to the cache directory of the cvtest1 application on the android emulator.  The code below loads this image, runs the canny edge detection algorithm and then writes the results back to a .png file in the same directory.  

    Put this code just below the code from the previous step and alter it to match your own files/directories.
     
        String inputFileName="simm_01";
        String inputExtension = "jpg";
        String inputDir = getCacheDir().getAbsolutePath();  // use the cache directory for i/o
        String outputDir = getCacheDir().getAbsolutePath();
        String outputExtension = "png";
        String inputFilePath = inputDir + File.separator + inputFileName + "." + inputExtension;


        Log.d (this.getClass().getSimpleName(), "loading " + inputFilePath + "...");
        Mat image = Imgcodecs.imread(inputFilePath);  
        Log.d (this.getClass().getSimpleName(), "width of " + inputFileName + ": " + image.width());
        // if width is 0 then it did not read your image.


        // for the canny edge detection algorithm, play with these to see different results
        int threshold1 = 70;
        int threshold2 = 100;

        Mat im_canny = new Mat();  // you have to initialize output image before giving it to the Canny method
        Imgproc.Canny(image, im_canny, threshold1, threshold2);
        String cannyFilename = outputDir + File.separator + inputFileName + "_canny-" + threshold1 + "-" + threshold2 + "." + outputExtension;
        Log.d (this.getClass().getSimpleName(), "Writing " + cannyFilename);
        Imgcodecs.imwrite(cannyFilename, im_canny);

12.  Run your application.  Your emulator should create a black and white "edge" image.  You can use the Android Device Monitor 
to retrieve the output or write an activity to show it.



  [1]: http://i.stack.imgur.com/KJOHU.jpg
  [2]: http://i.stack.imgur.com/7NgDd.jpg
  [3]: http://i.stack.imgur.com/eOoy4.png
  [4]: http://i.stack.imgur.com/vpSVn.png
  [5]: http://i.stack.imgur.com/57IxS.jpg
  [6]: http://i.stack.imgur.com/FzGnD.jpg
  [7]: http://i.stack.imgur.com/m6pTt.jpg
  [8]: http://i.stack.imgur.com/tcBJy.png
  [9]: http://i.stack.imgur.com/Tqz9H.jpg



