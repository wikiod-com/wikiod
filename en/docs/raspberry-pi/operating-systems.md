---
title: "Operating systems"
slug: "operating-systems"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This section contains information about various operating systems available for the Raspberry Pi's.

## Raspbian
The most popular operating system for the Raspberry Pi is a Debian based Raspbian.

It is officially supported by the Raspberry Pi Foundation.

Raspbian can be downloaded from official Raspberry Pi site in one of two variants:
* With desktop environment
* Lite- Minimal image 

Starting September 2016 Raspbian is shipped with PIXEL desktop environment ("Pi Improved Xwindows Environment, Lightweight")



## Yocto based operating systems
It is possible to create own Linux distribution using Yocto Project.

For Raspberry Pi- there is a specific layer [meta-raspberrypi][1] that needs to be used to create an image.


  [1]: http://git.yoctoproject.org/cgit/cgit.cgi/meta-raspberrypi

## Windows 10 IoT Core
Windows 10 IoT Core is available only for Raspberry Pi 2 and 3. It is important to note that this is not a full version of windows as one might expect, it is designed specifically for IoT (Internet of Things) applications. It will not run most standard windows applications, and does not have a start menu. It boots to a page with some example code and videos, and has a command line interface.

## OSMC
OSMC is a free and open source media player based on Linux. 

## LibreELEC
Linux distribution with features required to run [Kodi][1].


  [1]: https://kodi.tv/about/

## Kali
A fully featured OS designed around penetration testing.

Download Link:

https://www.offensive-security.com/kali-linux-arm-images/

Requirements:

 1. 8GB Class 10 SD Card Minimum
 2. 16GB Class 10 SD Card Recommended

Default Login:

    Username: root
    Password: toor

Notes

 1. Install gparted and expand partition to full size
 2. Run apt-get install kali-linux-full to get all the standard tools available
 3. Runs XFCE Desktop Environment

