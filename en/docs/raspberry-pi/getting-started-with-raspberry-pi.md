---
title: "Getting started with Raspberry Pi"
slug: "getting-started-with-raspberry-pi"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## SD card Setup
Start by installing the OS on the MicroSD card, either NOOBS or Raspbian, both provided by the Raspberry Pi foundation, available on [their site][1]. NOOBS, which stands for New Out Of Box Software, is designed for beginners, and is the easiest to install on your SD card. You can either follow their [official instructions][2], or see below.

Format the SD card with a single partition of FAT32, using either the system drive management tool or a third party tool, such as the SD Association's [SD Formatter 4.0][3], or [GParted][4]. Download the NOOBS [ZIP file][5] and unzip it. next, copy the files over to the partition that you just created. Ensure that you eject it before disconnecting it.


  [1]: https://www.raspberrypi.org/downloads/
  [2]: https://www.raspberrypi.org/learning/software-guide/quickstart/
  [3]: https://www.sdcard.org/downloads/formatter_4/index.html
  [4]: http://gparted.org/
  [5]: https://www.raspberrypi.org/downloads/noobs/

## Raspberry Pi Installation - Windows
**Pretext:**  
These are detailed instructions on how to set up a Raspberry Pi with the **Raspbian** operating system.  
These instructions are somewhat Windows specific.  
Some installation steps may apply to other operating systems as well, but keep the former in mind.

**Contents**  

- Requirements
- Choosing an OS
- Installing the OS
- Basic Configuration


----------
**Requirements**

1. Raspberry Pi (version 1, 2 or 3 - any model)
2. Power supply (5V, 2000mAh usb adapter - recommended)
3. MicroSD card (Class 10 recommended with at least 8GB - see list of compatible cards here: [eLinux](http://elinux.org/RPi_SD_cards))
4. Computer with SD Card reader (or a USB SD Card reader)
5. Monitor / TV with HDMI Port
6. HDMI Cable
7. USB Keyboard
8. [Win32 Disk Imager](https://sourceforge.net/projects/win32diskimager/)


----------
**Choosing an Operating System**  

 - [NOOBS](https://www.raspberrypi.org/downloads/noobs/)  
A collection of different operating systems for the Raspberry Pi, it contains Raspbian. In addition to Raspbian, NOOBS also includes Pidora, LibreELEC, OSMC, RISK OS, Arch Linux, Windows 10 IOT Core, and a few other choices. It is important to note that not all of these choices are available on every model of Raspberry Pi (for example, Windows IOT Core is only available on Raspberry Pi 2 and newer) . Installing any other OS than Raspbian will require an ethernet connection on your Raspberry Pi.
- [Raspbian](https://www.raspberrypi.org/downloads/raspbian/)  
The officially supported OS for the Raspberry Pi, it is a port of the OS known as Debian. Pre-installed with educational software and with a large community - this is the recommended OS for the Raspberry Pi
-  [Windows 10 IOT Core](https://developer.microsoft.com/en-us/windows/iot)
A simplified version of windows specifically designed for IOT (Internet Of Things) devices. It is important to note this is not a full version of windows, and does not have things user might expect, such as a start menu and the ability to run most Windows applications.


----------
**Installing the Operating System**  

*SD card*
 1. Connect your SD Card to your computer
 2. Download the Operating System you have chosen in ***.img*** format   
(It might be compressed in a ***.zip*** file, which should be extracted after download)
 3. Open Win32DiskImager
 4. Select your SD Card
 5. Select your OS ***.img*** file
 6. Click `Write` 

*Raspberry Pi*

 1. Insert the SD Card into the Raspberry Pi
2. Connect your USB Keyboard
3. Connect your Monitor/TV
4. Connect an Ethernet cable (optional: but recommended!)
5. Plug in the USB Power cable in the Raspberry Pi

*Operating System*

If the OS was written correctly to the SD Card and the OS is a valid ARM Operating System, it should be installed automatically to the Raspberry Pi - with little to no interaction.    
(This does however depend greatly on what kind of OS that has been chosen)  
If you chose NOOBS an interface will show up and you will be able to choose what to install.


----------

**Basic Configuration**  

*Command Line:*  

The command `sudo raspi-config` gives you access to the Raspberry Pi configuration.  

Setup Options:

    1 Expand Filesystem              Ensures that all of the SD card storage is available to the OS
    2 Change User Password           Change password for the default user (pi)
    3 Enable Boot to Desktop/Scratch Choose whether to boot into a desktop environment, Scratch, or the command line
    4 Internationalisation Options   Set up language and regional settings to match your location
    5 Enable Camera                  Enable this Pi to work with the Raspberry Pi camera
    6 Add to Rastrack                Add this Pi to the online Raspberry Pi Map (Rastrack)
    7 Overclock                      Configure overclocking for your Pi
    8 Advanced Options               Configure advanced settings
    9 About `raspi-config`           Information about this configuration tool

- After a new and clean installation you are recommended to choose the first option: `Expand Filesystem`
- If you're European and not familiar with the English keyboard layout you can go to `Internationalisation Options` and change the keyboard language and more.
- It is recommended to avoid the `Overclock` menu as a beginner. Especially if you have the Raspberry Pi 3. The RPI 3 is known to get very hot, even if it is not overclocked.  
If you are thinking of overclocking the RPI 3, please obtain heatsinks or a fan for your board, to keep it from overheating.  
- Enabling SSH access can be done in the `Advanced Options` menu, the credentials will be the default username and password.

*GUI:*  

Installing Raspbian Jessie automatically boots into a graphical user interface, the aforementioned options are presented in the Options menu in the top left corner.  
It is recommended for beginners to keep using the GUI, but if you bought a Raspberry Pi to learn the command line. The option to boot into the command line can be found in the Settings menu.

**Note**  
Changing settings on the Raspberry Pi usually prompts a restart which happens when you accept the changes you've made.


----------
**Extra Info**  
Default Username: pi  
Default Password: raspberry  

Shutting down your Raspberry Pi is done by pulling the USB power plug.  
(Choosing shutdown in the Raspbian GUI will simply put the Raspberry Pi in a diminished state, still using power - but less)  
Restarting the Raspberry Pi is done by simply plugging in the cable after unplugging it.  
(Unless the reboot option is chosen in the GUI or `sudo reboot` in the command line.


----------

## Raspberry Pi Introduction - Hello World in C
## Pretext ##
This is an introduction to the Hello World program example on the Raspberry Pi written in C.   
The following example makes use of the command line interface and is set up as a step-by-step guide.  
Along with creating a Hello World program, the reader will be introduced to simple linux command line commands.  

***Note:*** This introduction was written for beginners. 



----------
## Hello World - My first program ##    
**First step:**  
Making a directory that will contain source code.  

 - Locate your home directory by writing the following command `cd`
- Make a new folder for your source code `mkdir programs`  
(Tip: Linux is case sensitive when managing file and directory names)
- Change to your new directory `cd programs`  


----------


**Second step:**  
Writing your first program.  

Linux systems offer a great variety of text editors, natively you will find Vim or Nano.  
This example will make use of the Nano text editor.  
- Create your source code file `nano helloworld.c`
- This will open an editor - exiting and saving will create the file in your folder.  

The following code is the source code for the Hello World program:  

    /* My first program */

    #include<stdio.h>

    int main()
    {
       printf("Hello World\n");
    }

 - After writing the code hit `ctrl + x` to exit the editor, hit `y` and then `enter` to save the changes.
- Write the command `ls` to check if the file is present in your directory.


----------


**Third step:**  
Compiling your first program.  


- To compile our source code file `helloworld.c` we need to use a compiler, we will use the   
***GNU Compiler Collection*** - also known as GCC.
- The following command compiles the source code to an executable binary program  
 `gcc helloworld.c -o myfirstprogram.bin`  

The source code file is offered as an argument to the GCC compiler and `-o` defines another argument expressing that we would like the compiler to output something.  
In this case we want it to output a `.bin` file that we named ourselves.


There is several other arguments you can use when compiling with GCC, an example would be  
 `-wall` which enables all warnings. This gives you information about any error GCC might encounter.  


----------


**Fourth step:**  
Running your first program.

- Running a program on the Raspberry Pi is done by adding `./` in front of the name of the program that you want to run.
- We can run our program by writing `./myfirstprogram.bin`  

The command should execute the program and produce `Hello World` in the console window.



## Login with IPv6
Usually we struggle through the process of making login in the Raspberry Pi using `ssh` or other similar tools. But we can make the process more pleasent.

Once your Raspberry Pi is connected to a network it gets an IPv4 address and an [IPv6](https://en.wikipedia.org/wiki/Link-local_address#IPv6) address, that is based on the NIC's MAC address. The good thing about this is that the IPv6 address doesn't change even if the network has a DHCP service.

That being said, let's discover our IPv6 address. You only need to login once using IPv4 or using a monitor (HDMI or VGA). Open a terminal and type:

~~~
ifconfig
~~~

You will see something like:

~~~
eth0      Link encap:Ethernet  HWaddr 00:1C:C0:AE:B5:E6  
          inet addr:192.168.0.1  Bcast:192.168.0.255  Mask:255.255.255.0
          inet6 addr: fe80::21c:c0ff:feae:b5e6/64 Scope:Link
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:41620 errors:0 dropped:0 overruns:0 frame:0
          TX packets:40231 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:21601203 (20.6 MiB)  TX bytes:6145876 (5.8 MiB)
          Interrupt:21 Base address:0xe000 
~~~

As you can see your IPv4 would look like `192.168.0.1` and your IPv6 would be `fe80::21c:c0ff:feae:b5e6` (notice the line that starts with `inet6 addr`).

So, with this information you can login using the `ssh` command with the following syntax:

~~~
ssh -6 pi@fe80::21c:c0ff:feae:b5e6%eth0
~~~

This approach seems more complicated, but the IPv6 address is defined as the `link local`, and, as it is based on the MAC address, unless you change it yourself, this will always work regardless of your IPv4 address.

## Proxy Configuration
If you are behind a proxy and need to connect to the internet, you can use:

    export http_proxy="http://username:password@host:port/"

For configuring the proxy inside `apt-get`:

    cd /etc/apt/apt.conf.d

Create a file named 10proxy:

    sudo nano 10proxy

Without authentication add the following line:

    Acquire::http::Proxy "http://yourproxyaddress:proxyport/";
    Acquire::https::Proxy "http://yourproxyaddress:proxyport/";

With authentication:

    Acquire::http::Proxy "http://username:password@yourproxyaddress:proxyport/";

The `/` at the very end was important. Without it it does not work.

## Raspberry Pi v2 and v3 Arch Linux Installation --- from Mac or Linux
One of the best Linux distributions currently for Raspberry Pi (from now on, "RPi") is [Arch Linux][arch-arm-w]. This web shows the installation for:

[arch-arm-w]: https://archlinuxarm.org/

+   [RPi2][rpi2-inst]. ARMv7 architecture (32 bits).

+   [RPi3][rpi3-inst]. There are two options:
    -   ARMv7 architecture (32 bits) or
    -   AArch architecture (64 bits).

[rpi2-inst]: https://archlinuxarm.org/platforms/armv7/broadcom/raspberry-pi-2
[rpi3-inst]: https://archlinuxarm.org/platforms/armv8/broadcom/raspberry-pi-3

This tutorial advices you _not_ to install the AArch option --- "Some of the hardware on the board may not work, or it may perform poorly".


