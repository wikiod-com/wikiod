---
title: "Getting started with cuda"
slug: "getting-started-with-cuda"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Let's launch a single CUDA thread to say hello
This simple CUDA program demonstrates how to write a function that will execute on the GPU (aka "device"). The CPU, or "host", creates CUDA threads by calling special functions called "kernels". CUDA programs are C++ programs with additional syntax.

To see how it works, put the following code in a file named `hello.cu`:

    #include <stdio.h>
    
    // __global__ functions, or "kernels", execute on the device
    __global__ void hello_kernel(void)
    {
      printf("Hello, world from the device!\n");
    }
    
    int main(void)
    {
      // greet from the host
      printf("Hello, world from the host!\n");
    
      // launch a kernel with a single thread to greet from the device
      hello_kernel<<<1,1>>>();
    
      // wait for the device to finish so that we see the message
      cudaDeviceSynchronize();
    
      return 0;
    }

(Note that in order to use the `printf` function on the device, you need a device that  has a compute capability of at least 2.0. See the [versions overview](https://www.wikiod.com/cuda/getting-started-with-cuda) for details.) 

Now let's compile the program using the NVIDIA compiler and run it:

    $ nvcc hello.cu -o hello
    $ ./hello
    Hello, world from the host!
    Hello, world from the device!

Some additional information about the above example: 
  - `nvcc` stands for "NVIDIA CUDA Compiler". It separates source code into host and device components. 
  - `__global__` is a CUDA keyword used in function declarations indicating that the function runs on the GPU device and is called from the host.
  - Triple angle brackets (`<<<`,`>>>`) mark a call from host code to device code (also called "kernel launch"). The numbers within these triple brackets indicate the number of times to execute in parallel and the number of threads.

## Prerequisites
To get started programming with CUDA, download and install the [CUDA Toolkit and developer driver][1]. The toolkit includes `nvcc`, the NVIDIA CUDA Compiler, and other software necessary to develop CUDA applications. The driver ensures that GPU programs run correctly on [CUDA-capable hardware][2], which you'll also need.

You can confirm that the CUDA Toolkit is correctly installed on your machine by running `nvcc --version` from a command line. For example, on a Linux machine,

    $ nvcc --version
    nvcc: NVIDIA (R) Cuda compiler driver
    Copyright (c) 2005-2016 NVIDIA Corporation
    Built on Tue_Jul_12_18:28:38_CDT_2016
    Cuda compilation tools, release 8.0, V8.0.32

outputs the compiler information. If the previous command was not successful, then the CUDA Toolkit is likely not installed, or the path to `nvcc` (`C:\CUDA\bin` on Windows machines, `/usr/local/cuda/bin` on POSIX OSes) is not part of your `PATH` environment variable.

Additionally, you'll also need a host compiler which works with `nvcc` to compile and build CUDA programs. On Windows, this is `cl.exe`, the Microsoft compiler, which ships with Microsoft Visual Studio. On POSIX OSes, other compilers are available, including `gcc` or `g++`. The official CUDA [Quick Start Guide][1] can tell you which compiler versions are supported on your particular platform.

To make sure everything is set up correctly, let's compile and run a trivial CUDA program to ensure all the tools work together correctly.

    __global__ void foo() {}

    int main()
    {
      foo<<<1,1>>>();

      cudaDeviceSynchronize();
      printf("CUDA error: %s\n", cudaGetErrorString(cudaGetLastError()));

      return 0;
    }

To compile this program, copy it to a file called test.cu and compile it from the command line. For example, on a Linux system, the following should work:

    $ nvcc test.cu -o test
    $ ./test
    CUDA error: no error

If the program succeeds without error, then let's start coding!

  [1]: https://developer.nvidia.com/cuda-downloads
  [2]: http://www.nvidia.com/object/cuda_gpus.htm

## Compiling and Running the Sample Programs
The NVIDIA installation guide ends with running the sample programs to verify your installation of the CUDA Toolkit, but doesn't explicitly state how. First check all the prerequisites. Check the default CUDA directory for the sample programs. If it is not present, it can be downloaded from the official CUDA website. Navigate to the directory where the examples are present.

    $ cd /path/to/samples/
    $ ls

You should see an output similar to:

    0_Simple     2_Graphics  4_Finance      6_Advanced       bin     EULA.txt
    1_Utilities  3_Imaging   5_Simulations  7_CUDALibraries  common  Makefile

Ensure that the `Makefile` is present in this directory. The `make` command in UNIX based systems will build all the sample programs. Alternatively, navigate to a subdirectory where another `Makefile` is present and run the `make` command from there to build only that sample.

Run the two suggested sample programs - `deviceQuery` and `bandwidthTest`:

    $ cd 1_Utilities/deviceQuery/
    $ ./deviceQuery 

The output will be similar to the one shown below:

    ./deviceQuery Starting...
    
     CUDA Device Query (Runtime API) version (CUDART static linking)
    
    Detected 1 CUDA Capable device(s)
    
    Device 0: "GeForce GTX 950M"
      CUDA Driver Version / Runtime Version          7.5 / 7.5
      CUDA Capability Major/Minor version number:    5.0
      Total amount of global memory:                 4096 MBytes (4294836224 bytes)
      ( 5) Multiprocessors, (128) CUDA Cores/MP:     640 CUDA Cores
      GPU Max Clock rate:                            1124 MHz (1.12 GHz)
      Memory Clock rate:                             900 Mhz
      Memory Bus Width:                              128-bit
      L2 Cache Size:                                 2097152 bytes
      Maximum Texture Dimension Size (x,y,z)         1D=(65536), 2D=(65536, 65536), 3D=(4096, 4096, 4096)
      Maximum Layered 1D Texture Size, (num) layers  1D=(16384), 2048 layers
      Maximum Layered 2D Texture Size, (num) layers  2D=(16384, 16384), 2048 layers
      Total amount of constant memory:               65536 bytes
      Total amount of shared memory per block:       49152 bytes
      Total number of registers available per block: 65536
      Warp size:                                     32
      Maximum number of threads per multiprocessor:  2048
      Maximum number of threads per block:           1024
      Max dimension size of a thread block (x,y,z): (1024, 1024, 64)
      Max dimension size of a grid size    (x,y,z): (2147483647, 65535, 65535)
      Maximum memory pitch:                          2147483647 bytes
      Texture alignment:                             512 bytes
      Concurrent copy and kernel execution:          Yes with 1 copy engine(s)
      Run time limit on kernels:                     Yes
      Integrated GPU sharing Host Memory:            No
      Support host page-locked memory mapping:       Yes
      Alignment requirement for Surfaces:            Yes
      Device has ECC support:                        Disabled
      Device supports Unified Addressing (UVA):      Yes
      Device PCI Domain ID / Bus ID / location ID:   0 / 1 / 0
      Compute Mode:
         < Default (multiple host threads can use ::cudaSetDevice() with device simultaneously) >
    
    deviceQuery, CUDA Driver = CUDART, CUDA Driver Version = 7.5, CUDA Runtime Version = 7.5, NumDevs = 1, Device0 = GeForce GTX 950M
    Result = PASS

The statement `Result = PASS` at the end indicates that everything is working correctly. Now, run the other suggested sample program `bandwidthTest` in a similar fashion. The output will be similar to:

    [CUDA Bandwidth Test] - Starting...
    Running on...
    
     Device 0: GeForce GTX 950M
     Quick Mode
    
     Host to Device Bandwidth, 1 Device(s)
     PINNED Memory Transfers
       Transfer Size (Bytes)    Bandwidth(MB/s)
       33554432            10604.5
    
     Device to Host Bandwidth, 1 Device(s)
     PINNED Memory Transfers
       Transfer Size (Bytes)    Bandwidth(MB/s)
       33554432            10202.0
    
     Device to Device Bandwidth, 1 Device(s)
     PINNED Memory Transfers
       Transfer Size (Bytes)    Bandwidth(MB/s)
       33554432            23389.7
    
    Result = PASS
    
    NOTE: The CUDA Samples are not meant for performance measurements. Results may vary when GPU Boost is enabled.

Again, the `Result = PASS` statement indicates that everything was executed properly. All other sample programs can be run in a similar fashion.


## Sum two arrays with CUDA
This example illustrates how to create a simple program that will sum two `int` arrays with CUDA.

A CUDA program is heterogenous and consist of parts runs both on CPU and GPU. 

The main parts of a program that utilize CUDA are similar to CPU programs and consist of
 - Memory allocation for data that will be used on GPU
 - Data copying from host memory to GPUs memory
 - Invoking kernel function to process data
 - Copy result to CPUs memory 

To allocate devices memory we use `cudaMalloc` function. To copy data between device and host `cudaMemcpy` function can be used.
The last argument of `cudaMemcpy` specifies the direction of copy operation. There are 5 possible types:

- `cudaMemcpyHostToHost` - Host -> Host
- `cudaMemcpyHostToDevice` - Host -> Device
- `cudaMemcpyDeviceToHost` - Device -> Host
- `cudaMemcpyDeviceToDevice` - Device -> Device
- `cudaMemcpyDefault` - Default based unified virtual address space

Next the kernel function is invoked. The information between the triple chevrons is the execution configuration, which dictates how many device threads execute the kernel in parallel. 
The first number (`2` in example) specifies number of blocks and second (`(size + 1) / 2` in example) - number of threads in a block. Note that in this example we add 1 to the size, so that we request one extra thread rather than having one thread responsible for two elements.

Since kernel invocation is an asynchronous function `cudaDeviceSynchronize` is called to wait until execution is completed.
Result arrays is copied to the host memory and all memory allocated on the device is freed with `cudaFree`.

To define function as kernel  `__global__` declaration specifier is used. This function will be invoked by each thread. 
If we want each thread to process an element of the resultant array, then we need a means of distinguishing and identifying each thread.
CUDA defines the variables `blockDim`, `blockIdx`, and `threadIdx`. The predefined variable `blockDim` contains the dimensions of each thread block as specified in the second execution configuration parameter for the kernel launch. 
The predefined variables `threadIdx` and `blockIdx` contain the index of the thread within its thread block and the thread block within the grid, respectively. Note that since we potentially request one more thread than elements in the arrays, we need to pass in `size` to ensure we don't access past the end of the array.

    #include "cuda_runtime.h"
    #include "device_launch_parameters.h"

    #include <stdio.h>

    __global__ void addKernel(int* c, const int* a, const int* b, int size) {
        int i = blockIdx.x * blockDim.x + threadIdx.x;
        if (i < size) {
            c[i] = a[i] + b[i];
        }
    }

    // Helper function for using CUDA to add vectors in parallel.
    void addWithCuda(int* c, const int* a, const int* b, int size) {
        int* dev_a = nullptr;
        int* dev_b = nullptr;
        int* dev_c = nullptr;

        // Allocate GPU buffers for three vectors (two input, one output)
        cudaMalloc((void**)&dev_c, size * sizeof(int));
        cudaMalloc((void**)&dev_a, size * sizeof(int));
        cudaMalloc((void**)&dev_b, size * sizeof(int));

        // Copy input vectors from host memory to GPU buffers.
        cudaMemcpy(dev_a, a, size * sizeof(int), cudaMemcpyHostToDevice);
        cudaMemcpy(dev_b, b, size * sizeof(int), cudaMemcpyHostToDevice);

        // Launch a kernel on the GPU with one thread for each element.
        // 2 is number of computational blocks and (size + 1) / 2 is a number of threads in a block
        addKernel<<<2, (size + 1) / 2>>>(dev_c, dev_a, dev_b, size);
        
        // cudaDeviceSynchronize waits for the kernel to finish, and returns
        // any errors encountered during the launch.
        cudaDeviceSynchronize();

        // Copy output vector from GPU buffer to host memory.
        cudaMemcpy(c, dev_c, size * sizeof(int), cudaMemcpyDeviceToHost);

        cudaFree(dev_c);
        cudaFree(dev_a);
        cudaFree(dev_b);
    }

    int main(int argc, char** argv) {
        const int arraySize = 5;
        const int a[arraySize] = {  1,  2,  3,  4,  5 };
        const int b[arraySize] = { 10, 20, 30, 40, 50 };
        int c[arraySize] = { 0 };

        addWithCuda(c, a, b, arraySize);

        printf("{1, 2, 3, 4, 5} + {10, 20, 30, 40, 50} = {%d, %d, %d, %d, %d}\n", c[0], c[1], c[2], c[3], c[4]);

        cudaDeviceReset();

        return 0;
    }


