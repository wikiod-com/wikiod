---
title: "OpenCV Installation"
slug: "opencv-installation"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

OpenCV Installation On Linux, Mac OS and Windows

## OpenCV Installation on Ubuntu
Source Link

Open the Terminal and write the following commands.

1-Update and upgrade package your system ubuntu:

    sudo su
    
    sudo apt-get -y update
    
    sudo apt-get -y upgrade
    
    sudo apt-get -y dist-upgrade
    
    sudo apt-get -y autoremove

2-Installing Dependenices:

    sudo apt-get install libopencv-dev

3-Build Tools for OpenCV Source code:

    sudo apt-get install build-essential checkinstall cmake pkg-config

4-Image I/O Libraries for OpenCV:

    sudo apt-get install libtiff5-dev libjpeg-dev libjasper-dev libpng12-dev zlib1g-dev libopenexr-dev libgdal-dev

5-Video I/O Libraries for OpenCV:

    sudo apt-get install libavcodec-dev libavformat-dev libmp3lame-dev libswscale-dev libdc1394–22-dev libxine2-dev libgstreamer0.10-dev libgstreamer-plugins-base0.10-dev libv4l-dev v4l-utils libfaac-dev libopencore-amrnb-dev libopencore-amrwb-dev libtheora-dev libvorbis-dev libxvidcore-dev libx264-dev x264 yasm

6-Parallelism and linear algebra libraries:

    sudo apt-get install libtbb-dev libeigen3-dev

7-Graphic User Interface Libraries:

    sudo apt-get install libqt4-dev libgtk2.0-dev qt5-default
    
    sudo apt-get install libvtk6-dev

8–Java Installation:

    sudo apt-get install ant default-jdk

9-Python Installation:

    sudo apt-get install python-dev python-tk python-numpy python3-dev python3-tk python3-numpy python-matplotlib
    
    sudo apt-get install python-opencv
    
    sudo apt-get install doxygen

10-Download OPENCV source code from Github:

    wget https://github.com/opencv/opencv/archive/3.2.0.zip

11-Decompress OPENCV Zip file:

    unzip 3.2.0.zip

12-Remove OPENCV Zip file:

    rm 3.2.0.zip

13-Build OPENCV:

    mv opencv-3.2.0 opencv
    
    cd opencv
    
    mkdir build
    
    cd build
    
    cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D WITH_TBB=ON -D BUILD_NEW_PYTHON_SUPPORT=ON -D WITH_V4L=ON -D INSTALL_C_EXAMPLES=ON -D INSTALL_PYTHON_EXAMPLES=ON -D BUILD_DOC=ON -D BULD_EXAMPLES=ON -D WITH_QT=ON -D WITH_OPENGL=ON -D WITH_EIGEN=ON -D FORCE_VTK=TRUE -D WITH_VTK=ON ..
    
    make -j4

sudo make install

    sudo sh -c 'echo "/usr/local/lib" > /etc/ld.so.conf.d/opencv.conf'
    
    sudo ldconfig

14-Finished Check your OpenCV Version Number:

    pkg-config — modversion opencv
    
    pkg-config — cflags opencv













