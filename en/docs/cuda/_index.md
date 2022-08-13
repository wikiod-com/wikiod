---
title : cuda Tutorial
slug : cuda-tutorial
weight : 9918
draft : false
images : []
type : docs
---

CUDA is a proprietary NVIDIA parallel computing technology and programming language for their GPUs.

GPUs are highly parallel machines capable of running thousands of lightweight threads in parallel.
Each GPU thread is usually slower in execution and their context is smaller. On the other hand, GPU is able to run several thousands of threads in parallel and even more concurrently (precise numbers depend on the actual GPU model).
CUDA is a C++ dialect designed specifically for NVIDIA GPU architecture. However, due to the architecture differences, most algorithms cannot be simply copy-pasted from plain C++ - they would run, but would be very slow.

## Terminology ##

- *host* -- refers to normal CPU-based hardware and normal programs that run in that environment
- *device* -- refers to a specific GPU that CUDA programs run in. A single host can support multiple devices.
- *kernel* -- a function that resides on the device that can be invoked from the host code.

## Physical Processor Structure ##

The CUDA-enabled GPU processor has the following physical structure:

- *the chip* - the whole processor of the GPU. Some GPUs have two of them.
- *streamming multiprocessor* (SM) - each chip contains up to ~100 SMs, depending on a model. Each SM operates nearly independently from another, using only global memory to communicate to each other.
- *CUDA core* - a single scalar compute unit of a SM. Their precise number depends on the architecture. Each core can handle a few threads executed concurrently in a quick succession (similar to hyperthreading in CPU).

In addition, each SM features one or more *warp schedulers*.
Each scheduler dispatches a single instruction to several CUDA cores.
This effectively causes the SM to operate in 32-wide [SIMD][1] mode.

## CUDA Execution Model ##

The physical structure of the GPU has direct influence on how kernels are executed on the device, and how one programs them in CUDA.
Kernel is invoked with a *call configuration* which specifies how many parallel threads are spawned.

- *the grid* - represents all threads that are spawned upon kernel call. It is specified as a one or two dimentional set of *blocks*
- *the block* - is a semi-independent set of *threads*. Each block is assigned to a single SM. As such, blocks can communicate only through global memory.
Blocks are not synchronized in any way. If there are too many blocks, some may execute sequentially after others.
On the other hand, if resources permit, more than one block may run on the same SM, but the programmer cannot benefit from that happening (except for the obvious performance boost).
- *the thread* - a scalar sequence of instructions executed by a single CUDA core. Threads are 'lightweight' with minimal context, allowing the hardware to quickly swap them in and out. Because of their number, CUDA threads operate with a few registers assigned to them, and very short stack (preferably none at all!).
For that reason, CUDA compiler prefers to inline all function calls to flatten the kernel so that it contains only static jumps and loops. Function ponter calls, and virtual method calls, while supported in most newer devices, usually incur a major performance penality.

Each thread is identified by a block index `blockIdx` and thread index within the block `threadIdx`.
These numbers can be checked at any time by any running thread and is the only way of distinguishing one thread from another.

In addition, threads are organized into *warps*, each containing exactly 32 threads.
Threads within a single warp execute in a perfect sync, in SIMD fahsion. 
Threads from different warps, but within the same block can execute in any order, but can be forced to synchronize by the programmer.
Threads from different blocks cannot be synchronized or interact directly in any way.

## Memory Organisation ##

In normal CPU programming the memory organization is usually hidden from the programmer.
Typical programs act as if there was just RAM. All memory operations, such as managing registers, using L1- L2- L3- caching, swapping to disk, etc. is handled by the compiler, operating system or hardware itself.

This is not the case with CUDA. While newer GPU models partially hide the burden, e.g. through the [Unified Memory][2] in CUDA 6, it is still worth understanding the organization for performance reasons.
The basic CUDA memory structure is as follows:

* *Host memory* -- the regular RAM. Mostly used by the host code, but newer GPU models may access it as well.
When a kernel access the host memory, the GPU must communicate with the motherboard, usually through the PCIe connector and as such it is relatively slow.
* *Device memory / Global memory* -- the main off-chip memory of the GPU, available to all threads.
* *Shared memory* - located in each SM allows for much quicker access than global. Shared memory is private to each block. Threads within a single block can use it for communication.
* *Registers* - fastest, private, unaddressable memory of each thread. In general these cannot be used for communication, but a few intrinsic functions allows to shuffle their contents within a warp.
* *Local memory* - private memory of each thread that *is* addressable. This is used for register spills, and local arrays with variable indexing. Physically, they reside in global memory.
* *Texture memory, Constant memory* - a part of global memory that is marked as immutable for the kernel. This allows the GPU to use special-purpose caches.
* *L2 cache* -- on-chip, available to all threads. Given the amount of threads, the expected lifetime of each cache line is much lower than on CPU.
It is mostly used aid misaligned and partially-random memory access patterns.
* *L1 cache* -- located in the same space as shared memory. Again, the amount is rather small, given the number of threads using it, so do not expect data to stay there for long. L1 caching can be disabled.


  [1]: https://en.wikipedia.org/wiki/SIMD
  [2]: https://devblogs.nvidia.com/parallelforall/unified-memory-in-cuda-6/

