---
title: "Linux Hello World Device driver"
slug: "linux-hello-world-device-driver"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## An empty kernel module
    #include <linux/init.h>
    #include <linux/module.h>

    /**
     * This function is called when the module is first loaded.
     */    
    static int __init hello_kernel_init(void)
    {
        printk("Hello, World!\n");
        return 0;
    }
    
    /**
     * This function is called when is called if and when the module is unloaded.
     */
    static void __exit hello_kernel_exit(void)
    {
        printk("Goodbye, cruel world...\n");
    }
    
    /* The names of the init/exit functions are arbitrary, and they are bound using the following macro definitions */
    module_init(hello_kernel_init);
    module_exit(hello_kernel_exit);
    
In order to write a Linux device driver (Character-device, Block-device, etc...), it is necessary to create a kernel module that has an entry and exit points.

By itself, the kernel module does nothing; it has no meaningful way to communicate with the userspace.
Using the entry point it is possible to create a new character-device, for example, which is then used to communicate with the userspace.

## Building and running the module
In order to compile the driver, it is necessary to have the Linux Kernel source tree.

Assuming the sources are at `/lib/modules/<kernel-version>`, the following Makefile will compile the file `driver.c` into the `driver.ko` Kernel Object

    obj-m := driver.o
    KDIR := /lib/modules/$(shell uname -r)/build/
    PWD := $(shell pwd)
    
    all:
        $(MAKE) -C $(KDIR) M=$(PWD) modules
Notice how this Makefile calls `make` in the build directory of the Kernel.

When the compilation step finishes successfully, the src directory of the driver will look somewhat like this:

    driver.c  driver.ko  driver.mod.c  driver.mod.o  driver.o  Makefile  modules.order  Module.symvers

In order to "run" the module, it is necessary to insert into the running kernel:

    $ insmod driver.ko
    $ dmesg | tail -n 1
    [133790.762185] Hello, World!

    $ rmmod driver.ko
    $ dmesg | tail -n 1
    [133790.762185] Goodbye, cruel world...
    

 

