---
title: "Inter-block communication"
slug: "inter-block-communication"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Blocks in CUDA operate semi-independently. There is no safe way to synchronize them all.
However, it does not mean that they cannot interact with each other in any way.


## Last-block guard
Consider a grid working on some task, e.g. a parallel reduction.
Initially, each block can do its work independently, producing some partial result.
At the end however, the partial results need to be combined and merged together.
A typical example is a reduction algorithm on a big data.

A typical approach is to invoke two kernels, one for the partial computation and the other for merging.
However, if the merging can be done efficiently by a single block, only one kernel call is required.
This is achieved by a `lastBlock` guard defined as:

<!-- if version [gte 2.0] -->
    __device__ bool lastBlock(int* counter) {
      __threadfence(); //ensure that partial result is visible by all blocks
      int last = 0;
      if (threadIdx.x == 0)
        last = atomicAdd(counter, 1);
      return __syncthreads_or(last == gridDim.x-1);
    }
<!-- end version if -->
<!-- if version [gte 1.1] -->
    __device__ bool lastBlock(int* counter) {
      __shared__ int last;
      __threadfence(); //ensure that partial result is visible by all blocks
      if (threadIdx.x == 0) {
        last = atomicAdd(counter, 1);
      }
      __syncthreads();
      return last == gridDim.x-1;
    }
<!-- end version if -->

With such a guard the last block is guaranteed to see all the results produced by all other blocks and can perform the merging.

    __device__ void computePartial(T* out) { ... }
    __device__ void merge(T* partialResults, T* out) { ... }

    __global__ void kernel(int* counter, T* partialResults, T* finalResult) {
        computePartial(&partialResults[blockIdx.x]);
        if (lastBlock(counter)) {
          //this is executed by all threads of the last block only
          merge(partialResults,finalResult);
        }
    }
    
    
Assumptions:
- The counter must be a global memory pointer, initialized to 0 *before* the kernel is invoked.
- The `lastBlock` function is invoked uniformly by all threads in all blocks
- The kernel is invoked in one-dimentional grid (for simplicity of the example)
- `T` names any type you like, but the example is not intended to be a template in C++ sense

## Global work queue
Consider an array of work items. The time needed for an each work item to complete varies greatly.
In order to balance the work distribution between blocks it may be prudent for each block to fetch the next item only when previous one is complete.
This is in contrast to a-priori assigning items to blocks.

    class WorkQueue {
    private:
      WorkItem* gItems;
      size_t totalSize;
      size_t current;
    public:
      __device__ WorkItem& fetch() {
        __shared__ WorkItem item;
        if (threadIdx.x == 0) {
          size_t itemIdx = atomicAdd(current,1);
          if (itemIdx<totalSize)
            item = gItems[itemIdx];
          else
            item = WorkItem::none();
        }
        __syncthreads();
        return item; //returning reference to smem - ok
      }
    }

Assumptions:
- The WorkQueue object, as well as gItem array reside in global memory
- No new work items are added to the WorkQueue object in the kernel that is fetching from it
- The `WorkItem` is a small representation of the work assignment, e.g. a pointer to another object
- `WorkItem::none()` static member function creates a `WorkItem` object that represents no work at all
- `WorkQueue::fetch()` must be called uniformly by all threads in the block
- There are no 2 invocations of `WorkQueue::fetch()` without another `__syncthreads()` in between. Otherwise a race condition will appear!

The example does not include how the initialize the `WorkQueue` or populate it. It is done by another kernel or CPU code and should be pretty straight-forward.

