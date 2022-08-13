---
title: "Getting started with Ubuntu"
slug: "getting-started-with-ubuntu"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
## What is Ubuntu ##
---
Ubuntu is an open source software platform, but colloquially, when Ubuntu is referred to it's mainly toward the Ubuntu operating system. Ubuntu is based on Debian and uses the same package management system (deb and apt).

## Installation ##
---
So you want to give Ubuntu a try! That's great. First off, let's grab the Ubuntu .iso file that you'll be needing to install the operating system on your system. Note, an .iso file is an image file that we can burn to a USB/CD. Think of it as a snapshot of the Ubuntu Operating System that we'll burn onto some media disk.

- Head over to Ubuntu's download page [Here](http://www.ubuntu.com/download/desktop)
- Grab an .iso to USB burner. 
    - [Pendrivelinux](http://www.pendrivelinux.com/liveusb-install-live-usb-creator/) is a popular choice
    - [Rufus](https://rufus.akeo.ie/) is another popular alternative
- Load up whichever program and load in the .iso file. 
- Burn the image to the USB (be careful you choose the correct USB!)
- Once the burn is complete, eject safely
- Plug the USB into the system that you wan't to install Ubuntu on, flip the switch and follow the on screen instructions

## Keeping Ubuntu and your packages up to date
Once you installed Ubuntu, you might want to get the latest patches and updates. Using Ubuntu's easy to use package manager Aptitude, the OS along with all future packages that is installed using this manner can be kept up to date.

 1. Download the latest package lists by refreshing information from the repositories:
`sudo apt-get update`
[![apt-get update][1]][1]


 2. Then proceed to run the following command to review which packages can upgraded: `sudo apt-get upgrade`
[![apt-get upgrade][2]][2]


 3. Assuming you are satisfied with the lists of packages that can be upgraded, enter `y` to start the installation process.

Also you could just hit enter, when presented with a choice the capitalized choice is the default, and is selected if you hit enter and type nothing. 

[![yes][3]][3]


  [1]: http://i.stack.imgur.com/fUgNP.png
  [2]: http://i.stack.imgur.com/RYrJy.png
  [3]: http://i.stack.imgur.com/FIC2D.png

