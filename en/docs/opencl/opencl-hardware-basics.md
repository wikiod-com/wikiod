---
title: "OpenCL hardware basics"
slug: "opencl-hardware-basics"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This topic introduces some of the underlying core mechanics of parallel computing which are needed to fully understand and utilize OpenCL.

## Threads  and Execution
The key of parallelism is to use multiple threads to solve a problem (duh.) but there are some differences to classical multithreaded programming in how threads are organized.

First lets talk about your typical GPU, for simplicities sake I'll focus on 

A GPU has many processing cores, which make it ideal to execute many threads in parallel. Those cores are organized in Streaming Processors (SM, NVidia term), of which a GPU has a given number.

All threads running inside a SM are called a 'thread block'. There can be more threads on an SM than it has cores. The number of cores defines the so called 'Warp size' (NVidia term). Threads inside a thread block are sheduled in so called 'warps'.

A quick example to follow up:
A typical NVidia SM has 32 processing cores, thus its warp size is 32. If my thread block now has 128 threads to run, they will be shedulled in 4 warps (4 warps * 32 warp size = 128 threads).

The warp size is rather important when choosing the number of threads later on.

All threads inside a single warp share a single instruction counter. That means those 32 threads are truly synchronized in that every thread executes every command at the same time. Here lies a performance pitfall: This also applies to branching statements in your kernel!

Example: I have a kernel that has an if statement and two branches. 16 of my threads inside a warp will execute branch one, the other 16 branch two. Up until the if statement, all threads inside the warp are in sync. Now half of them choose a different branch. What happens is that the other half will lay dormant until the wrong statement has finished executing on the first 16 threads. Then those threads will be dormant until the other 16 threads finished their branch.

As you can see, bad branching habits can severely slow down your parallel code, because both statements get executed in the worst case. If all threads inside a warp decide they only need one of the statements, the other one is completely skipped and no delay occurs.

Syncing threads is also not a simple matter. You can **only** sync threads withing a single SM. Everything outside the SM is unsyncable from inside the kernel. You'll have to write seperate kernels and launch them one after the other.

## GPU Memory
The GPU offers six different memory regions. They differ in their latency, size and accessibility from different threads.

- Global Memory: The largest memory available and one of the few ones to exchange data with the host. This memory has the highest latency and is available for all threads.
- Constant Memory: A read only part of the global memory, which can only be read by other threads. Its advantage is the lower latency compared to the global memory
- Texture Memory: Also a part of constant memory, specifically designed for textures
- Shared Memory: This memory region is placed close to the SM and can only accessed by a single thread block. It offers way lower latency than the global memory and a bit less latency than the constant memory.
- Registers: Only accessible by a single thread and the fastest memory of them all. But if the compiler detects that there are not enough Registers for the kernel needs, it will outsource variables to local memory.
- Local Memory: A thread-only accessible part of memory in the global memory region. Used as a backup for registers, to be avoided if possible.

## Memory access
The typical scenario for your memory usage is to store the source data and the processed data in the global memory. When a threadblock starts, it first copies all relevant parts into the shared memory before getting their parts into the registers.

Memory access latency also depends on your memory strategy. If you blindly access data you will get the worst performance possible.

The different memories are organized in so-called 'banks'. Each memory request for a bank can be handled in a single clock cycle. The number of banks in the shared memory equals the warp size. The memory speed can be increased by avoiding conflicting bank access inside a single warp.

To copy shared memory from or to global memory the fastest way is to 'align' your memory calls. This means that the first thread in a warp should access the first element in the bank of both the shared and global memory. The second thread the second element and so on. This call will be optimized into a single memory transfer instruction which copies the whole bank to the target memory in one go.

