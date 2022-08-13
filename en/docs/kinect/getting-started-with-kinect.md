---
title: "Getting started with kinect"
slug: "getting-started-with-kinect"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting kinect set up or installed.Setting Up a Kinect Sensor

**Kinect for Windows 1.5, 1.6, 1.7, 1.8**

Here are some simple steps to get your Kinect sensor up and running.

> Step 1: Mount the Sensor on a Stable Surface

Place the sensor on a stable surface in a location where it will not fall or be struck during use. Here are some other tips.

    Do not place the Kinect on or in front of a speaker or on a surface that vibrates or makes noise.
    Keep the Kinect out of direct sunlight.
    Use the Kinect within its specified operating temperature range of 41 to 95 degrees Fahrenheit (5 to 35 degrees Celsius). If the sensor is exposed to an environment outside of its prescribed temperature range, turn it off and allow the temperature to stabilize within the specified range before you use the sensor again.
    Do not tilt a Kinect on its base. Manually tilting the Kinect can damage the sensor. The Kinect tilt angle is controlled by software; see Kinect Explorer - WPF C# Sample for an example.

> Step 2: Install the Kinect for Windows SDK

    If you haven't done so already, download and install the free SDK from the Kinect for Windows download page.
    If you haven't done so already, download and install the free Developer Toolkit from the Kinect for Windows download page.

> Step 3: Plug in your Kinect sensor

    Connect the power supply for your Kinect to an external power source.
    Connect the Kinect to a USB port on your PC and wait for Windows to recognize it.
    All the drivers, including audio, will load seamlessly.

When you install the Kinect for Windows SDK, the installation package includes the Kinect drivers. When the Kinect drivers are installed on a Windows-based PC, a Kinect that is plugged into the PC's USB port appears as a multicomponent USB device.

There is a known issue regarding USB host controller compatibility.

After loading the drivers, you need to set up your development environment. If you are using Visual Studio, continue with Configuring the Development Environment to help you create your first C# or C++ application.

**Troubleshooting Tips**

The following tips will help you get started using your Kinect:

    If a non-Microsoft driver for the Kinect is installed on your computer, the Kinect for Windows drivers might not install or function correctly. To fix this, uninstall the non-Microsoft drivers before installing the Kinect for Windows SDK.
    Connect the power supply for the Kinect to an external power source; if the Kinect has only power from the USB connection, it will be minimally functional and light the LED, but it must be connected to an external power source to be fully functional.
    No tools are required for calibration of audio and video.
    Your Kinect should be the only device plugged into a USB hub on your computer. If you have more than one Kinect, connect them to different USB controllers. If 2 hubs are connected to the same controller, only 1 Kinect can work at a time.
    The Kinect is protected from overheating by a fan. It is controlled by the sensor's firmware, which turns off the camera at 90 degrees Celsius. There is no software interface for applications to control the fan.
    Reasonable lighting, neither extremely dark nor extremely bright, is important for capturing images with the RGB camera. Incandescent, fluorescent, and natural lighting provide no special obstacles, but do not point an intense or constant light source at the camera because this can blind the RGB sensor.
    The depth sensor functions adequately in typical and reduced lighting, although in near darkness there is increased noise in the signal.
    The depth sensor reads depth information from reflected light. Objects that are highly reflective (mirrors and shiny metal) or highly absorptive (fluffy and/or dark materials) may not be registered by the depth sensor as successfully as other objects.


[Source][1]

**Linux**

**Ubuntu/Debian
Official packages**

Starting from Ubuntu 11.10 (Oneiric) and Debian 7 (Wheezy), Ubuntu and Debian provide official packages of libfreenect. You can install them easily in a console:

    $ sudo apt-get install freenect

In ***Ubuntu 12.04*** the **gspca** kernel driver prevent **libfreenect** from claiming the **Kinect** device in user-mode. Either remove and blacklist the module

    $ sudo modprobe -r gspca_kinect 
    $ sudo modprobe -r gspca_main
    $ echo "blacklist gspca_kinect" |sudo tee -a /etc/modprobe.d/blacklist.conf

or install a recent versions of **libfreenect** that can automatically detach the kernel driver by adding Florian Echtler libtisch PPA (see below) with updated **libfreenect** packages for Ubuntu 12.04.

The **freenect** device is accessible to any user belonging to the group 'plugdev'. By default, a desktop user belongs to the plugdev group but if you need to add them to the group:

    $ sudo adduser $USER plugdev

then log out and log in again
NeuroDebian repository

If you want a recent version of libfreenect no matter which version of Debian or Ubuntu you use, backports of the latest release of libfreenect for all supported version of Debian and Ubuntu (namely Ubuntu Lucid(10.04), Maverick (10.10), Natty (11.04), Oneiric (11.10) and Debian Squeeze and Wheezy at the time of writing) are available on [NeuroDebian][2] repository . The packages available there are created by the maintainers of the official Debian package and follows the standards of Debian/Ubuntu.

To enable the NeuroDebian repository:

    $ wget -O- http://neuro.debian.net/lists/$(lsb_release -cs).us-nh | sudo tee /etc/apt/sources.list.d/neurodebian.sources.list
    $ sudo apt-key adv --recv-keys --keyserver pgp.mit.edu 2649A5A9
    $ sudo apt-get update

Installing **libfreenect** is the same as before:

    $ sudo apt-get install freenect

Make sure your user belongs to the plugdev group (The default for a desktop user) to access the device without the root privileges. If it is not the case, add them by:

    $ sudo adduser $USER plugdev

and log out and log in again
libtisch PPA

An Ubuntu launchpad ppa for Lucid(10.04), Maverick (10.10), Natty (11.04), Oneiric (11.10) and Pangolin (12.04) is available at this [link][3].



to use it, open a console and execute:

    $ sudo add-apt-repository ppa:floe/libtisch
    $ sudo apt-get update

After that, you can use:

    $ sudo apt-get install libfreenect libfreenect-dev libfreenect-demos

This will install libfreenect, the development headers, and the demo applications.

After that, you need to add yourself to the 'video' group and log back in. The package already includes the necessary rules for the udev daemon so that accessing the device will be possible for users in the group video.

    $ sudo adduser $USER video

be sure to log out and back in. You don't need to reboot, just plug in the kinect device right now (if it was already connected, unplug and plug back in).


> To start the demo applications use:

    $ freenect-glview

Problems with accessing device

> In case of problems, run

    $ lsusb | grep Xbox

which should list 3 devices

    

    > lsusb | grep Xbox                                                 Bus
    > 001 Device 021: ID 045e:02ae Microsoft Corp. Xbox NUI Camera Bus 001
    > Device 019: ID 045e:02b0 Microsoft Corp. Xbox NUI Motor Bus 001 Device
    > 020: ID 045e:02ad Microsoft Corp. Xbox NUI Audio

If they are not present, run

    echo -1 | sudo tee -a /sys/module/usbcore/parameters/autosuspend

and reconnect Kinekt and `Kinect Camera` should be listed

You find all demo applications starting with the freenect- prefix.
Ubuntu Manual Install

> Quick copy-paste instructions to get up-and-running instantly:

    sudo apt-get install git-core cmake libglut3-dev pkg-config build-essential libxmu-dev libxi-dev libusb-1.0-0-dev
    git clone git://github.com/OpenKinect/libfreenect.git
    cd libfreenect
    mkdir build
    cd build
    cmake ..
    make
    sudo make install
    sudo ldconfig /usr/local/lib64/
    sudo freenect-glview

> Note: If you getting an error saying apt-get cannot find libglut3, you
> might be on a newer version of Ubuntu that has freeglut3-* instead of
> libglut3-*, so your initial apt-get install would look like:

    sudo apt-get install git-core cmake freeglut3-dev pkg-config build-essential libxmu-dev libxi-dev libusb-1.0-0-dev

To use Kinect as a non-root user do the following:

    sudo adduser $USER video

Also make a file with rules for the Linux device manager:

    sudo nano /etc/udev/rules.d/51-kinect.rules

Copy and paste:

    # ATTR{product}=="Xbox NUI Motor"
    SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02b0", MODE="0666"
    # ATTR{product}=="Xbox NUI Audio"
    SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02ad", MODE="0666"
    # ATTR{product}=="Xbox NUI Camera"
    SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02ae", MODE="0666"
    # ATTR{product}=="Xbox NUI Motor"
    SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02c2", MODE="0666"
    # ATTR{product}=="Xbox NUI Motor"
    SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02be", MODE="0666"
    # ATTR{product}=="Xbox NUI Motor"
    SUBSYSTEM=="usb", ATTR{idVendor}=="045e", ATTR{idProduct}=="02bf", MODE="0666"

Be sure to log out and back in.

If you can't access or still need root privileges to use your device: in some cases there might be conflicts between permissions of two drivers installed (libfreenect and primesense). If this is your case, try reinstalling primesense's sensor driver and keep only primesense's rules file `/etc/udev/rules.d/55-primesense-usb.rule`s, removing the `/etc/udev/rules.d/51-kinect.rules` file if you created it. 


  [1]: https://msdn.microsoft.com/en-us/library/hh855356.aspx
  [2]: http://neuro.debian.net
  [3]: https://launchpad.net/~floe/+archive/libtisch




