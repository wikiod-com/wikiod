---
title: "Host memory interaction"
slug: "host-memory-interaction"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

This topic highlights different ways to put data somewhere where your device can access it.

## Reading an array
To read an array from the device back to the host, one calls

    clEnqueueReadBuffer($queue, $memobj, $blocking, $offset, $size, $target, 0, null, null);

The $queue is the CommandQueue which was used to allocate the memory on the device. The $memobj contains the address to the device memory, $offset and $size further define from where and how much data is copied. The $target is a pointer to the host memory where the data will be stored in. The $target needs to be allocated and have an appropriate size.

## Reading a Texture
Reading an image is almost like reading an array. The only difference beeing that the size and offset need to be three-dimensional.

    clEnqueueReadImage($queue, $memobj, $blocking, $offset, $size, $stride, $slice_pitch, $target, 0, null, null);

The $stride defines how many bytes a row has. Normally this is just width * (bytes per pixel), but someone might want to change that to align the data with the memory banks. The same goes for $slice_pitch, only that this value is for the third dimension.


## Writing a 2D Texture
To copy a texture to the device there are two steps necessary

1. Allocate the memory on the device
2. Copy the image to the device


      _mem = clCreateImage2D($context, $mem_flags, $image_format, $width, $height, $stride, $source, &err);

The $mem_flags define how the memory is allocated. It can be either read only, write only or both. Additionally you can define where and how the memory is allocated. $width, $height and $stride are pretty self explanatory.

If your mem_flags copy the data, you're done. If you want to do that manually at a later point, you'll need to call another function when you are ready.

    err = clEnqueueWriteImage($queue, _mem, $blocking, $offset, $size, $stride, $slice_pitch, $source, 0, null, null);

The $offset and $size define the image region which you want to copy to the target memory. The $stride defines how many bytes a row has. Normally this is just width * (bytes per pixel), but someone might want to change that to align the data with the memory banks. The same goes for $slice_pitch, only that this value is for the third dimension. Both $stride and $slice_pitch have to match your input data.

## Memory flags
When allocating Memory you have the option to choose between different modes:
- Read only memory
- Write only memory
- Read/Write memory

Read-only memory is allocated in the __constant memory region, while the other two are allocated in the normal __global region.

In addition to the accessibility you can define where your memory is allocated.
- Not specified:
Your memory is allocated on the device memory as you would expect. The $source pointer can be set to null.
- CL_MEM_USE_HOST_PTR: This tells the device that the data is in the system RAM and should not be moved. Instead the data is manipulated directly in the ram.
- CL_MEM_COPY_HOST_PTR: Tells the device to copy all values at the given address to device memory or, using CL_MEM_ALLOC_HOST_PTR, to a seperate memory region in system ram.
- CL_MEM_ALLOC_HOST_PTR: Tells the device to allocate space at the system ram. If used as the only parameter, the $source pointer can be set to null.

Speed-wise, access to device global memory is the fastest one. But you also need to call it twice to copy data. Using the Host-pointer is the slowest one, while Alloc_host_ptr offers a higher speed.

When using use_host_ptr, the device does exactly that: It uses your data in the system ram, which of course is paged by the os. So every memory call has to go through the cpu to handle potential pagefaults. When the data is available, the cpu copies it into pinned memory and passes it to the DMA controller using precious cpu clock cycles. On the contrary, alloc_host_ptr allocates pinned memory in the system ram. This memory is placed outside of the pageswap mechanism and therefore has a guaranteed availability. Therefore the device can skip the cpu entirely when accessing system ram and utilize DMA to quickly copy data to the device.

## Writing an array
Writing an array consists of two steps:
1. Allocating the memory
2. Copying the data

To allocate the memory, a simple call to

    _mem = clCreateBuffer($queue, $mem_flags, $size, $host_ptr, &err);

is enough. If you decided to copy the host pointer via the mem_flags, you are done. Otherwise you can copy the data whenever you like with

    err = clEnqueueWriteBuffer($queue, _mem, $blocking, $offset, $size, $source, 0, null, null);

