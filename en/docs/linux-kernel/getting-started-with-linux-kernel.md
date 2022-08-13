---
title: "Getting started with linux-kernel"
slug: "getting-started-with-linux-kernel"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Linux kernel source code can be found in https://www.kernel.org/

# Download extract and enter to the kernel directory
Type these commands step by steps in your terminal.(Choose the appropriate version you needed instead of linux-4.7.tar.gz )

    wget http://www.kernel.org/pub/linux/kernel/v4.7/linux-4.7.tar.gz
    tar zxvf linux-4.7.tar.gz
    cd linux-4.7

`make menuconfig` will select the features required for the kernel.
 Old kernel configurations can be copied by using old `.config` file and executing `make oldconfig`. Also we can use  `make xconfig` as a graphical version of the configuration tool.

# Build the dependencies, compile the kernel and modules.

    make dep
    make bzImage
    make modules
    make modules_install 


Alternatively if you want to reconfigure the old kernel and re compile it, execute the below commands:

    make mrproper
    make menuconfig
    make dep
    make clean
    make bzImage
    make modules
    make modules_install

Then copy the kernel, `system.map` file to `/boot/vmlinuz-4.7`

create a `.conf` file with the below content

    image = /boot/vmlinuz-4.7
    label = "Linux 4.7"

Then execute `lilo -v` to modify the boot sector and reboot.

