---
title: "User Space and Kernel Space"
slug: "user-space-and-kernel-space"
draft: false
images: []
weight: 9989
type: docs
toc: true
---


The RAM can be divided into two parts. The kernel space and user space. The kernel runs in the kernel space, which no other programs can access. User programs have to run in user space. User space is a form of sand-boxing, where user programs can only access to memory that allocated to them so that they can't mess up other programs and the kernel. To use the system resource, a program uses system calls to access a certain part of the kernel space and back to user space when the call return.

## Switching between user space and kernel space
Kernel manages operating system resources. User program can only access to those resources by making system calls to the kernel. System call is similar to an API of kernel, which in term, runs kernel tasks your program needs.

    str = "something" // run on user space
    x = x + 1 // run on user space
    file.write(str) // switch to kernel space
    y = x + 4 // switch back to user space

## Exam CPU time allocation between user space and kernel space
Use `top` command to exam CPU time allocation between user space and kernel space. 
[![enter image description here][1]][1]

Explanation:
1. 24.8 us (user space): 24.8% of CPU time is spent on user process.
2. 0.5 sy (system): 0.5% of CPU time is spent on kernel space.
3. ni (niceness): the ratio of CPU time spent on low priority processes.
4. id (idle): the ratio of CPU time spent on idle processes (during the period of time, CPU can not do anything).
5. wa (wait): the ratio of CPU time spent on waiting for I/O (during the period of time, CPU can not do anything). 
6. hi (hardware interrupt): the ratio of CPU time spent on responding hardware interruption.
7. si (software interrupt): the ratio of CPU time spent on responding software interruption.
8. st (stole time): the ratio of CPU time stolen by virtual machine.


  [1]: https://i.stack.imgur.com/eILbr.png

## Exam CPU time spent on one process
Use time command.

    time ./perl-timeout-example 100.100.100
    
    We could not ping the desired address!
    
    real    0m5.0013s
    user    0m0.004s
    sys     0m0.008s

 - Real: Total time from start to finish of the call, including the CPU time spent on other processes.
 - User: The amount of CPU time spent in user space.
 - Sys: The amount of CPU time spent in the kernel space.

Normally, user + sys is the total CPU time spent on such process. It is smaller than real. But in multicore CPU, user + sys is the total CPU time spent on this process. It could be large than real.

