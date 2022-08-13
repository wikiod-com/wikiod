---
title: "Getting started with opencl"
slug: "getting-started-with-opencl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is OpenCL?
OpenCL is short for **Open** **C**omputing **L**anguage. OpenCL is a Framework for parallel programming across heterogeneous platforms, called *compute devices*, ranging from CPUs over GPUs to more special platforms like FPGAs. OpenCL provides a standard interface for parallel computing on these compute devices but also inter-device parallelism. It specifies a programming language, based on C99, and minimum requirements of basic functions implemented on OpenCL capable devices. OpenCL furthermore describes an abstract computing and memory model, being as general as possible to make the reuse of code between different platforms straightforward. 

## Prerequisites
If you have a modern CPU or graphics card (GPU) inside your machine, chances are you have everything ready for first steps in OpenCL. 
Finding out if your processor is OpenCL capable can be usually done via the manufacturer's homepage, a good first start is the official documentation at 

https://www.khronos.org/conformance/adopters/conformant-products#opencl




## What is OpenCL?
Open Computing Language (OpenCL) is a framework for writing programs that execute on CPUs, GPUs, and other parallel processors and accelerators.

OpenCL specifies a programming language (based on C) that provides access to named on-chip memory, a model for executing tasks in parallel, and the ability to synchronize those tasks.

## C# implementation of OpenCL 1.2: number of platforms for an AMD system in 64-bit windows
OpenCL is low level api so it must be implemented in "C space" first. For that, one needs to download header files from Khronos' site. My hardware is AMD and capable of version 1.2, downloading 

    opencl.h 
    cl_platform.h 
    cl.h 
    cl_ext.h 
    cl_egl.h 
    cl_dx9_media_sharing.h 
    cl_d3d10.h 
    cl_d3d11.h 
    cl_gl.h 
    cl_gl_ext.h 
    cl.hpp

from [this page][1]

should be enough for C++ bindings so after adding these files to your project and setting proper binary(and library) file locations(

> $(AMDAPPSDKROOT)\lib\x86_64 for 64-bit amd library (amd app sdk's libraries are preferred)

, 

> C:\Windows\SysWOW64 for 64-bit opencl.dll (.so file if ICD is of a Linux system)

 for example but different for Intel-Nvidia), you can start querying a list of platforms(amd,intel,xilinx,nvidia) after installing proper drivers(such as crimson for amd). Drivers are for running opencl application(using ICD), libraries and header files are for development to be in short.

To query platforms:


    #define __CL_ENABLE_EXCEPTIONS
    #include "stdafx.h"
    #include <vector>
    #include <CL/cl.hpp>

    extern "C"
        {
           // when this class is created, it contains a list of platforms in "platforms" field.
           class OpenClPlatformList
           {
               public:
                   std::vector<cl::Platform> platforms;
                   int platformNum;
                   OpenClPlatformList()
                   {
                       platforms= std::vector< cl::Platform>();
                       cl::Platform::get(&platforms);
                       platformNum= platforms.size();
                   }
            };


            // this is seen from C# when imported. Creates an object in memory.
            __declspec(dllexport)
                OpenClPlatformList * createPlatformList()
            {
                return new OpenClPlatformList();
            }

            __declspec(dllexport)
                int platformNumber(OpenClPlatformList * hList)
            {
                return hList->platformNum;
            }


            __declspec(dllexport)
                void deletePlatformList(OpenClPlatformList * p)
            {
                if (p != NULL)
                    delete p;
                p = NULL;
            }


        }

could be built into a dll(such as OCLImplementation.dll)

and to use it from C# side,

    using System;
    using System.Collections.Generic;
    using System.Runtime.InteropServices;


    namespace WrapperCSharp
    {
        public class WrapperCSharp
        {
            [DllImport("OCLImplementation", CallingConvention = CallingConvention.Cdecl)]
            private static extern IntPtr createPlatformList();
    
            [DllImport("OCLImplementation", CallingConvention = CallingConvention.Cdecl)]
            private static extern int platformNumber(IntPtr hList);
    
            [DllImport("OCLImplementation", CallingConvention = CallingConvention.Cdecl)]
            private static extern void deletePlatformList(IntPtr hList);
        }
    }

ofcourse the dll must be seen by the C# project, simply putting it near executable of project solves it.

Now, if sample computer has at least one opencl-capable platform,

    IntPtr platformList = createPlatformList(); // just an address in C-space
    int totalPlatforms = platformNumber(platformList); // AMD+NVIDIA systems should have "2"
    deletePlatformList(platformList); //

totalPlatforms variable must have at least "1" value. Then you can use platforms variable in C-space using additional functions to iterate through all platforms to query all devices such as CPU,GPU and special purpose accelerators such as phi or some fpga.

One does not simply write all these C++ to C# wrappers for time-critical projects. There are many wrappers written for C#, Java and other languages. For java, there is "Aparapi" that is the "java bytecode to opencl-c" converter api that takes what you write purely in java to a gpu-parallel version on the fly so it is somewhat portable.


  [1]: https://www.khronos.org/registry/cl/

## OpenCL and C#
For C# there exist many wrappers that offer an interface to communicate with OpenCL.

 - OpenCL.NET:
This is one of the most low level wrappers out there. It offers a complete implementation of the OpenCL API for C# without adding any abstraction at all. So C\C++ examples are easily ported for this library. The only project page is currently on codeplex, which shuts down on 15.12.2017 but the package is available on NuGet

https://openclnet.codeplex.com/
 - NOpenCL:
This library offers an abstract interface between C# and OpenCL. 

> The short-term goal is providing an easy-to-use abstract layer which provides access to the full capability of OpenCL without sacrificing performance.

https://github.com/tunnelvisionlabs/NOpenCL
 - Cloo:

> Cloo is an open source, easy to use, managed library which enables .NET/Mono applications to take full advantage of the OpenCL framework.

https://sourceforge.net/projects/cloo/

