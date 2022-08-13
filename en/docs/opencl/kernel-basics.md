---
title: "Kernel Basics"
slug: "kernel-basics"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This topic aims to explain the fundamentals of writing kernels for opencl

## Grayscale kernel
Lets build a kernel to generate a grayscale image. We will use image data which is defined using uints for each component and with order RGBA.

    __constant sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                                    CLK_ADDRESS_CLAMP_TO_EDGE |
                                    CLK_FILTER_LINEAR;
    
    __kernel void Grayscale(__read_only image2d_t input, __write_only image2d_t output) {
        int2 gid = (int2)(get_global_id(0), get_global_id(1));
        int2 size = get_image_dim(input);
    
        if(all(gid < size)){
            uint4 pixel = read_imageui(input, sampler, gid);
            float4 color = convert_float4(pixel) / 255;
            color.xyz = 0.2126*color.x + 0.7152*color.y + 0.0722*color.z;
            pixel = convert_uint4_rte(color * 255);
            write_imageui(output, gid, pixel);
        }
    }

Now lets walk through that code step by step. The first line creates a variable in the __constant memory region of type sampler_t. This sampler is used to further specify the access to our image data. Please refer to the Khronos Docs for a full documentation.

We allocated the input as read_only and the output as write_only before we called our kernel so we add those modifiers here.

image2d and image3d are always allocated on the global memory, therefore we can omit the __global modifier here.

Then we get our thread id which defines the pixel we are going to convert to grayscale. We also query the size to make sure that our thread is not accessing unallocated memory. This will definitley crash your kernel if you forget that.

After we made sure that we are a legitimate thread, we read our pixel out of our input image. We then convert it to float to avoid loss of decimal places, do some calculations, convert it back and write it into the output.

## Kernel Skelleton
Lets walk through the most simple kernel there is and some variations of it

    __kernel void myKernel() {
    }

A kernel which can be started from your main code is identified by the __kernel keyword. A Kernel function can only have return type void.

    __kernel void myKernel(float a, uint b, byte c) {

    }

Of course you can create more functions which are not exposed as kernels. In this case you can just omit the __kernel modifier.

A function may expose variables like any other C/C++ function would. The only difference is when you want to reference memory. This applies to all pointers whether they are arguments or used in code.

    float*  ptr;

is a pointer to a memory region only the executing thread has access to. In fact it is the same as

    __private float* ptr;

There are four different memory region modifiers available. Inside the kernel you usually don't have to worry about it, but for arguments this is essential.

- __global: This modifier refers to a pointer which is placed in the global memory
- __constant: refers to a constant memory pointer
- __local: refers to a shared memory pointer
- __private: refers to a local memory pointer

In addition we can define how we want to access the memory
- no modifier: read and write
- __read_only
- __write_only

Those flags have to match the way we allocated the memory buffer back on our host.

## Kernel ID
To properly work with the data each thread needs to know its position in the threadblock/global thread pool. This can be archieved with

    get_local_id($dim);
    get_global_id($dim);

Those two functions return the position of the thread relative to the threadblock or all threads.

    get_working_dim();

Gets the total number of dimensions the kernel was launched with.

    get_local_size($dim);
    get_global_size($dim);

Gets the total number of threads in the threadblock or in total for a given dimension.

Caveat: make always sure that your thread is not exceeding your data size. This is very likely to happen and should always be checked for.

## Vectors in OpenCL
Each fundamental opencl type has a vector version. You can use the vector type by appending the number of desired components after the type. Supported number of components are 2,3,4,8 and 16. OpenCL 1.0 does not offer three components.

You can initialize any vector using two ways:
- Provide a single scalar
- Satisfy all components


    float4 a = (float4)(1); //a = (1, 1, 1, 1)
or

    float4 b = (float4)(1, 2, 3, 4);
    float4 c = (float4)(1, (float3)(2));

or any other combination of vectors which satisfy the number of components. To access the elements of a vector you can use different methods. You can either use indexing:

    a[0] = 2;

or use literals. The advantage of literals is that you can combine them any way you want, well do that in a moment. You can access all vector components with

    a.s0 = 2; // same as a[0] = 2

you can also combine multiple components into a new vector

    a.s02 = (float2)(0, 0); // same as  a[0] = 0; a[2] = 0; or even a.s20 = (float2)(0, 0)

you can change the order of the components in any way you want.

    a.s1423 = a.s4132; // flip the vector

but you cannot do something like

    a.s11 = ... // twice the same component is not possible

There are some convenient shorthands for accessing vector components. The following shorthands only apply to sizes 2, 4, 8 and 16

    a.hi //=a.s23 for vectors of size 4, a.4567 for size 8 and so on.
    a.lo //=a.s01
    a.even //=a.s02
    a.odd //=a.13

For vector sizes 2,3 and 4 there are some additional shorthands

    a.x //=a.s0
    a.y //=a.s1
    a.z //=a.s2
    a.w //=a.s3

## Gamma Correction kernel
Lets look at a gamma correction kernel

    __constant sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                                    CLK_ADDRESS_CLAMP_TO_EDGE |
                                    CLK_FILTER_LINEAR;
    
    __kernel void Gamma(__read_only image2d_t input, __write_only image2d_t output, __constant float gamma) {
        int2 gid = (int2)(get_global_id(0), get_global_id(1));
        int2 size = get_image_dim(input);
    
        if(all(gid < size)){
            uint4 pixel = read_imageui(input, sampler, gid);
            float4 color = convert_float4(pixel) / 255;
            color = pow(color, (float4)(gamma));
            pixel = convert_uint4_rte(color * 255);
            write_imageui(output, gid, pixel);
        }
    }

Now lets walk through that code step by step. The first line creates a variable in the __constant memory region of type sampler_t. This sampler is used to further specify the access to our image data. Please refer to the Khronos Docs for a full documentation.

We allocated the input as read_only and the output as write_only before we called our kernel so we add those modifiers here.

image2d and image3d are always allocated on the global memory, therefore we can omit the __global modifier here. Our gamma value is located in __constant memory, so we specify that too.

Then we get our thread id which defines the pixel we are going to gamma correct. We also query the size to make sure that our thread is not accessing unallocated memory. This will definitley crash your kernel if you forget that.

After we made sure that we are a legitimate thread, we read our pixel out of our input image. We then convert it to float to avoid loss of decimal places, do some calculations, convert it back and write it into the output.



