---
title: "Enable SSH on Raspbian"
slug: "enable-ssh-on-raspbian"
draft: false
images: []
weight: 9868
type: docs
toc: true
---

Previous versions of Raspbian (prior to November 2016) had SSH enabled by default. They also had a default username (pi) and password (raspberry) this was done to make first time setup for new users easier. But this obviously represents a large security hole. The new release (November 2016) disables SSH by default, and will also display a warning if SSH is enabled without changing the the default password. 

The contents of the ssh file do not matter - you can create an empty file like we did above. 

During boot up the Pi will look for this file. If it exists SSH will be enabled and the file deleted. 




## Enable SSH using a Windows computer
To enable SSH create a file called ssh in the /boot directory of your SD card.

 1. Identify the drive letter associated with your SD card. 
 2. Open a command prompt (press **Win+R** on your keyboard to open the Run window. Then, type **cmd**)
 3. Enter the following at the command prompt (replacing DriveLetter with the letter you identified in step 1):

    echo.>DriveLetter:/ssh

 4. Press <kbd>Enter</kbd>

Verify that your SD card contains a file called ssh.

 1. Enter the following at the command prompt (again replacing DriveLetter with the letter you identified in step 1)

    dir DriveLetter:

 6. Press <kbd>Enter</kbd>

You should see an ssh file in the directory listing.

To avoid the security warning mentioned above (re: SSH enabled with the default password)  change the default password for the Pi user. This can be done either from the command line with the `passwd`, the raspi-config script, or settings menu in the Pixel desktop.

 



## Use Raspberry Pi as headless system
Above example helps us how to turn on SSH on Pi. This example is the prerequisite of above example. 

**For Linux OS**:
Open the terminal and write down this `ssh pi@your local IP address`. Then you will need to provide the password.

**Note**: Here **pi**: your raspberry pi **username** and **your local IP address**: the IP address of your device. You will get this IP address from your router, DHCP client list. However, this topic is out of scope.

**For Windows OS**: 
We can use [Putty][1] to login via ssh. At Host Name write your Raspberry pi IP address. 


[![Image][3]][3]


After pressing **Open**. You should provide your **username** and **password**. After the successful attempt, you will get this screen.

[![enter image description here][2]][2]
 


  [1]: https://the.earth.li/~sgtatham/putty/latest/w32/putty.exe
  [2]: https://i.stack.imgur.com/PALku.png
  [3]: https://i.stack.imgur.com/lmby0.png

