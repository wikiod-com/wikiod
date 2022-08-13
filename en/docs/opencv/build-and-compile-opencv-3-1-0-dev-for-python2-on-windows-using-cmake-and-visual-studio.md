---
title: "Build and Compile opencv 3.1.0-dev for Python2 on Windows using CMake and Visual Studio"
slug: "build-and-compile-opencv-310-dev-for-python2-on-windows-using-cmake-and-visual-studio"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Building and Compiling `opencv 3.1.0-dev` to get an access for **non free modules** can be a headache for some people especially on Windows machine. Unlike Ubuntu, setting up opencv for Windows takes some time and requires a couple pf dependencies to be installed first before building and compiling.

The programs you should download and install before going further in any step are:

 1. [Python 2.7.x][1] or [Python 3.x.x][1]
 2. [CMake][2]

**If you are going to download either Python for Win32, you should also download CMake for Win32 even if you are using a 64-bit machine.**

**It's recommended to download the 32-bit programs because some Python libraries are only supported for 32-bit machines, so to stay away from troubles, just install everything in 32-bit version.**

 3. [Visual Studio Community 2013][3] 

 4. [Numpy][4] for Python2.7 Win32

After Installing all the above dependencies, ***restart*** your PC and then you will be ready to continue to the next step.


----------


> **Step 2:**

If you are not the type of person who prefers to read, you can watch this [tutorial][5]. The tutorial takes you from here to the end of this documentation.

You will need to get **opencv** and **opencv_contrib** from **github**. You can find both at:

 1. [opencv][6]
 2. [opencv_contrib][7]

Create a directory named opencv-3.1.0 where in this director you will make another two directories one for the *build* and one for the *sources*. You will put the two downloaded zip files in the sources file after extraction.

For example your opencv-3.1.0 directory is located in the C drive, so you will have three paths:

 1. `C:\opencv-3.1.0` 
 2. `C:\opencv-3.1.0\build`
 3. `C:\opencv-3.1.0\sources`

The third directory will include two paths:

 1. `C:\opencv-3.1.0\sources\opencv`
 2. `C:\opencv-3.1.0\sources\opencv_contrib`

**Now it's done with preparation. Lets make some useful stuff.**

----------

> **Step 3:**

Run CMake as an administator. A window like this will appear and you will have to provide two directories one for the sources and the other for where the opencv will be compiled. The below image can help you more better than words.

[![cmake][8]][8]

Next click **configure** and you will be promoted to provide the generators; i.e. compilers; for opencv. You have to provide the `cl.exe` located in Microsoft Visual Studio 2013. Click **specify native generators** and a pop up window like the following will appear,

The paths will be something like this: `C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/cl.exe`. Provide your path for both C and C++ fields. Click finish and wait until configuring is done. You should get zero errors if you were following all the previous steps correctly.

[![compile][9]][9]

After CMake finishes configuring, you will see new items appearing in the CMake window that are highlighted in red. It will be something like:

[![after configuring][10]][10]

Check the builds you need by clicking on the small square box. Search for `OPENCV_EXTRA_MODULES_PATH` line and provide the modules directory within opencv_contrib within the sources directory.

[![extra modules][11]][11]

Once you have finished all you need and provided the path for the extra modules press configure again to update. The previously highlighted lines will no longer be highlighted and new fields will be highlighted in red instead.

[![new highlighted in red][12]][12]

Also check the boxes for whatever you need to build.

> Make sure that ***BUILD_opencv_contrib_world*** and ***BUILD_opencv_world*** are both **unchecked**. There is probably a bug where an error occurs when any of the latter are checked.

At the end of this step click **Generate** and you will be done with CMake and you can close it. *If there are no errors, you will get a message at the end of the lower pane saying **Generating done**.*

[![generating done][13]][13]

----------

> **Step 4:**

Open up the build directory located in opencv-3.1.0 and you will find a bunch of new folders and files inside it. It was an empty folder at the beginning of this process.

You will only deal with **`OpenCV.sln`** file and don't do anything with the rest files. Open this file with the version that used while compiling in the CMake in the previous step. It has to be **`Visual Microsoft 2013`**.

[![sln file][14]][14]

*When you open the .sln file, please be patient as it takes some time to prepare everything for building.* When **Ready** is steady (not changing) you can start building your targets. Start building as numbered in the image below. Also make sure that the `Solution Configuration` is `Release` not `Debug`.

[![start building][15]][15]

----------

> **Step 5:**

When building is finished, you will need to copy and paste a couple of files from the build directory into the `Python27` directory.

Search for `cv2.pyd` file and copy it to the `site-packages` directory in `Python27`. The `cv2.pyd` should be present in `C:\opencv-3.1.0\build\lib\Release`. After that, copy **only** the **.dll** files inside `C:\opencv-3.1.0\build\bin\Release` into the parent directory of `Python27` at this location `C:\Python27`.

At the end of this step, restart you PC.

----------

> **Verification:**

Open IDLE and within the Python shell type:

    >>> import cv2
    >>> print cv2.__version__
    3.1.0-dev
[![verification][16]][16]
 


  [1]: https://www.python.org/downloads/
  [2]: https://cmake.org/download/
  [3]: https://www.visualstudio.com/en-us/news/vs2013-community-vs.aspx
  [4]: https://sourceforge.net/projects/numpy/files/NumPy/1.11.1/
  [5]: https://www.youtube.com/watch?v=MXqpHIMdKfU
  [6]: https://github.com/opencv/opencv
  [7]: https://github.com/opencv/opencv_contrib
  [8]: http://i.stack.imgur.com/uX5T9.png
  [9]: http://i.stack.imgur.com/PckxE.png
  [10]: http://i.stack.imgur.com/sAtWA.png
  [11]: http://i.stack.imgur.com/IRbAX.png
  [12]: http://i.stack.imgur.com/7umpI.png
  [13]: http://i.stack.imgur.com/GPdC3.png
  [14]: http://i.stack.imgur.com/q3hnI.png
  [15]: http://i.stack.imgur.com/sG7YH.png
  [16]: http://i.stack.imgur.com/LVxkN.png

## Reading Image and Converting into grayscale
    import cv2
    import numpy as np
    
    img = cv2.imread('<your_image>')
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    
    cv2.imshow('image', img)
    cv2.imshow('gray', gray)
    
    cv2.waitKey(0)
    cv2.destroyAllWindows()



