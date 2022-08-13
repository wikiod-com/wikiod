---
title: "Getting started with linux-device-driver"
slug: "getting-started-with-linux-device-driver"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting linux-device-driver set up or installed.

## Hello World device driver
hello_world.c

    #include <linux/module.h>
    #include <linux/kernel.h>
    #include <linux/init.h>
    
    #define AUTHOR  "Bruce Lee"
    #define DESC    "Hello World driver"
    
    static int __init init(void)
    {
        printk(KERN_DEBUG "Hello World\n");
        return 0;
    }

    static void __exit deinit(void)
    {
        printk(KERN_DEBUG "Goodbye World\n");
    }
    
    module_init(init);
    module_exit(deinit);
    
    MODULE_LICENSE("GPL");
    MODULE_AUTHOR(AUTHOR);
    MODULE_DESCRIPTION(DESC);

Makefile:

    KDIR ?= /lib/modules/`uname -r`/build
    obj-m += hello_world.o
    
    all:
            make -C $(KDIR) M=$(PWD) modules
    
    clean:
            make -C $(KDIR) M=$(PWD) clean

How to compile:

    $ make

How to insert the module:

    $ sudo insmod hello_world.ko

How to see print messages

    $ dmesg

How to list module/s

    $ lsmod | grep hello_worls

How to remove the module

    $ sudo rmmod hello_world

