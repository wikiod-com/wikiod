---
title: "OpenCL basic setup"
slug: "opencl-basic-setup"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Before utilizing OpenCL, one has to set up their code to use it. This topic focuses on how to get opencl up and running in your project and execute a basic kernel. The examples are based on the C# wrapper OpenCL.NET but as the wrapper adds no abstraction to OpenCL the code will probably run with very few changes on C/C++ aswell.

Calls in C# may look as follows: 'Cl.GetPlatformIDs'. For the C-Style OpenCL Api you would call 'clGetPlatformIDs' and for the C++ style one 'cl::GetPlatformIDs'

- NVidia, AMD and Intel have slightly different implementations of OpenCL but the known differences are (for my experience) limited to parenthesis requirements and implicit casts. Sometimes NVidia will crash your kernel while trying to figure out the correct overload for a method. In this case it helps to offer an explicit cast to aid the GPU. The problem was observed for runtime-compiled kernels.

- To get more information on the used calls in this topic, it is enough to google 'OpenCL ' followed by the function name. The Khronos group has a complete documentation
    on all parameters and datatypes available on their website.

## Initializing the target Device
OpenCL Kernels can be either executed on the GPU or the CPU. This allows for fallback solutions, where the customer may have a very outdated system. The programmer can also choose to limit their functionality to either the CPU or GPU.

To get started using OpenCL, you'll need a 'Context' and a 'Device'. Both are structures defined by the OpenCL API (also known as cl::Context or clContext & ~Device) and define the used target processor.

To get your device and context, you need to query a list of available platforms, which each can host multiple devices. A Platform represents your physical GPU and CPU, while a device can further distinguish the contained computing units. For GPUs, most platforms will only have one device. But a CPU may offer an additional integrated GPU beside its CPU capabilities.

The context manages memory, command queues, the different kernels and programs. A context can either be limited to a single device but also reference multiple devices.

A quick API note before we start coding: Almost every call to OpenCL gives you an error value, either as return value or via a ref-value (pointer in C). Now lets get started.

    ErrorCode err;
    var platforms = Cl.GetPlatformIDs(out err);
    if(!CheckError(err, "Cl.GetPlatformIDs")) return;
    foreach (var platform in platforms) {
        foreach (var device in Cl.GetDeviceIDs(platform, DeviceType.Gpu, out err)) {
            if(!CheckError(err, "Cl.GetDeviceIDs")) continue;
            [...]
        }
    }

This code snippet queries all available GPU devices on the system. You can now add them to a list or start your context directly with the first match. The 'CheckError(...)' function is a simple utility, that checks whether the error code has the success-value or a different one and can offer you some logging. It is recommended to use a seperate function or macro, because you will call that a lot.

ErrorCode is just an enum on the datatype cl_int for C#, C/C++ can compare the int value with predefined error constants as listed here:
https://www.khronos.org/registry/OpenCL/sdk/1.0/docs/man/xhtml/errors.html

You also might want to check whether the device supports all needed features, otherwise your kernels might crash at runtime. You can query a device capability with

    Cl.GetDeviceInfo(_device, DeviceInfo.ImageSupport, out err)

This example asks the device whether it can execute image functions. For the next and final step we need to construct our context out of the collected devices.

    _context = Cl.CreateContext(null, 1, new[] { _device }, ContextNotify, IntPtr.Zero, out err);

Some stuff is going on here. For C/C++ folks, IntPtr is a pointer address in C#. I will concentrate on the important parts here.
 - The second parameter defines the number of devices you want to use
 - The third parameter is an array of those devices (or a pointer in C/C++)
 - And the third parameter is a function pointer for a callback function. This function will be used whenever errors happen inside the context.

For futher usage, you'll need to preserve your used devices and the context somewhere.

When you have finished all your OpenCL interaction you'll need to release the context again with

    Cl.ReleaseContext(_context);

## Compiling your Kernel
Kernels can be compiled at runtime on the target device. To do so, you need
 - the kernel source code
 - the target device on which to compile
 - a context built with the target device

A quick terminology update: A program contains a collection of kernels. You can think of a program as a complete C/C++/C# source file, while kernels are the different function members of that file.

First you'll need to create a program out of your source code.

    var program = Cl.CreateProgramWithSource(_context, 1, new[] { source }, null, out err);

You can combine multiple source files into one program and compile them together, which allows you to have kernels in different files and compile them together in one go.

In the next step you'll need to compile the program on your target device.

    err = Cl.BuildProgram(program, 1, new[] { _device }, string.Empty, null, IntPtr.Zero);

Now here comes a little caveat: The error code only tells you, whether the function call itself was successfull but not whether your code did actually compile. To verify that, we have to query some additional information

    BuildStatus status;
    status = Cl.GetProgramBuildInfo(program, _device, ProgramBuildInfo.Status, out err).CastTo<BuildStatus>();
    if (status != BuildStatus.Success) {
        var log = Cl.GetProgramBuildInfo(program, _device, ProgramBuildInfo.Log, out err);
    }

C/C++ people can ignore the cast at the end and just compare the returned integer with the corresponding constant.

The first call checks whether our build was actually successfull. If not we can retreive a log and see exactly where things went wrong. See the remarks for some common pitfals regarding different platforms.

Once the program is built, you need to extract your different kernels out of the compiled program. To do so you create your kernels with

    _kernel = Cl.CreateKernel(_program, kernel, out err);

where 'kernel' is a string of the kernel name. When you are finished with your kernel, you need to release it with

    Cl.ReleaseKernel(_kernel);

## Creating a command queue
To initiate any operation on your devices, you'll need a command queue for each device. The Queue keeps track of different calls you did to the target device and keeps them in order. Most commands can also be executed either in blocking or non-blocking mode.

Creating a queue is pretty straightforward:

    _queue = Cl.CreateCommandQueue(_context, _device, CommandQueueProperties.None, out err);

The basic interaction with your command queue is to enqueue different operations you want to perform, e.g. copy data to and from your device and launch a kernel.

When you have finished using the command queue you need to release the queue with a call to

    Cl.ReleaseCommandQueue(_queue);

## Executing the Kernel
So now we come down to the real stuff, executing your kernels on the parallel device. Please read about the hardware basics to fully understand the kernel dispatching.

First you'll need to set the kernel arguments before actually calling the kernel. This is done via

    err = Cl.SetKernelArg(_kernel, $argumentIndex, $argument);

If you don't set every argument before launching the kernel, the kernel will fail.

Before we actually launch our kernel, we need to calculate the 'global work size' and the 'local work size'.

the global work size is the total number of threads that will be launched on your GPU. The local work size is the number of threads inside each thread block. The local work size can be omitted if the kernel does not need any special requirements. But if the local work size is given, the global work size has to be a multiple of the local work size.

The work sizes can either be one-dimensional, two dimensional or three dimensional. The choice on how many dimensions you want is entirely up to you and you can pick whatever suits your algorithm best.

Now that we decided on our work sizes we can call the kernel.

    Event clevent;
    err = Cl.EnqueueNDRangeKernel(_queue, _kernel, $dimensions, null, $globalWorkSize, $localWorkSize, 0, null, out clevent);

The $dimensions define our desired number of dimensions, $globalWorkSize is an array of size $dimensions with the global Work size and the same for $localWorkSize. The last argument gives you an object which represents your currently executed kernel.

