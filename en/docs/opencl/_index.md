---
title : opencl Tutorial
slug : opencl-tutorial
weight : 9951
draft : false
images : []
type : docs
---

This section provides an overview of what opencl is, and why a developer might want to use it.

It should also mention any large subjects within opencl, and link out to the related topics.  Since the Documentation for opencl is new, you may need to create initial versions of those related topics.

----------------------------------------
Opencl is an api that puts gpus,cpus and some other accelerators(like a pcie-fpga) into  good use of C99-like computations but with a very wide concurrency advantage. Once installation and basic implementation is done, only simple changes in a kernel string(or its file) applies an algorithm to N hardware threads automagically.

A developer might want to use it because it will be much easier to optimize for memory space or speed than doing same thing on opengl or direct-x. Also it is royalty-free. Concurrency within a device is implicit so no need for explicit multi-threading for each device. But for multi-device configurations, a cpu-multi-threading is still needed. For example, when a 1000 threaded job is sent to a cpu, thread synchronization is handled by driver. You just tell it how big a workgroup should be(such as 256 each connected with virtual local memory) and where synchronization points are(only when needed).

Using gpu for general purpose operations is nearly always faster than cpu. You can sort things quicker, multiply matrices 10x faster and left join in-memory sql tables in "no" time. Any 200$ desktop-grade gpu will finish quicker in a physics(finite-element-method fluid) workload than any 200$ cpu. Opencl makes it easier and portable. When you're done
 working in C#, you can easily move to java-opencl implementation using same kernels and C++ project(ofcourse using JNI with extra C++ compiling). 

For the graphics part, you are not always have to send buffers between cpu and gpu. You can work purely on gpu using "interop" option in context creation part. With interop, you can prepare geometries at the limit  performance of gpu. No pci-e required for any vertex data. Just a "command" is sent through, and work is done only inside of graphics card. This means no cpu-overhead for data. Opencl prepares geometry data, opengl renders it. CPU becomes released. For example, if a single thread of cpu can build a 32x32 verticed sphere in 10000 cycles, then a gpu with opencl can build 20 spheres in 1000 cycles. 



----------------------------------------

