---
title: "Creation and usage of Kernel Threads"
slug: "creation-and-usage-of-kernel-threads"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This topic discusses how to create and use kernel threads.

## Creation of kernel threads
kern_thread.c

    #include <linux/module.h>
    #include <linux/kernel.h>
    #include <linux/init.h>
    #include <linux/kthread.h>
    #include <linux/sched.h>

    #define AUTHOR        "Nachiket Kulkarni"
    #define DESCRIPTION    "Simple module that demonstrates creation of 2 kernel threads"

    static int kthread_func(void *arg)
    {
    /* Every kthread has a struct task_struct associated with it which is it's identifier.
    * Whenever a thread is schedule for execution, the kernel sets "current" pointer to 
    * it's struct task_struct.
    * current->comm is the name of the command that caused creation of this thread
    * current->pid is the process of currently executing thread 
    */
        printk(KERN_INFO "I am thread: %s[PID = %d]\n", current->comm, current->pid);
        return 0;
    }

    static int __init init_func(void)
    {
        struct task_struct *ts1;
        struct task_struct *ts2;
        int err;
  
        printk(KERN_INFO "Starting 2 threads\n");

    /*struct task_struct *kthread_create(int (*threadfn)(void *data), void *data, \
     *                         const char *namefmt, ...);
     * This function creates a kernel thread and starts the thread.
     */
        ts1 = kthread_run(kthread_func, NULL, "thread-1");
        if (IS_ERR(ts1)) {
            printk(KERN_INFO "ERROR: Cannot create thread ts1\n");
            err = PTR_ERR(ts1);
            ts1 = NULL;
            return err;
        }
    
        ts1 = kthread_run(kthread_func, NULL, "thread-1");
        if (IS_ERR(ts1)) {
            printk(KERN_INFO "ERROR: Cannot create thread ts1\n");
            err = PTR_ERR(ts1);
            ts1 = NULL;
            return err;
        }
    
        printk(KERN_INFO "I am thread: %s[PID = %d]\n", current->comm, current->pid);
        return 0;
    }

    static void __exit exit_func(void)
    {
        printk(KERN_INFO "Exiting the module\n");
    }

    module_init(init_func);
    module_exit(exit_func);

    MODULE_AUTHOR(AUTHOR);
    MODULE_DESCRIPTION(MODULE_AUTHOR);
    MODULE_LICENSE("GPL");

Makefile

    obj-m += kern_thread.o

    all:
        make -C /lib/module/$(shell uname -r)/build M=$(PWD) modules
    clean:
        make -C /lib/module/$(shell uname -r)/build M=$(PWD) clean

Upon inserting the .ko, it printed:

    Starting 2 threads
    I am thread: thread-1[PID = 6786]
    I am thread: insmod[PID = 6785]
    I am thread: thread-2[PID = 6788]



