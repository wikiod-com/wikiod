---
title: "Parallel reduction (e.g. how to sum an array)"
slug: "parallel-reduction-eg-how-to-sum-an-array"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Parallel reduction algorithm typically refers to an algorithm which combines an array of elements, producing a single result.
Typical problems that fall into this category are:

- summing up all elements in an array
- finding a maximum in an array

In general, the parallel reduction can be applied for any binary [associative operator][1], i.e. `(A*B)*C = A*(B*C)`.
With such operator *, the parallel reduction algorithm repetedely groups the array arguments in pairs.
Each pair is computed in parallel with others, halving the overall array size in one step.
The process is repeated until only a single element exists.

<img src="http://i.stack.imgur.com/Uehc3.png" width="350">

If the operator is [commutative][2] (i.e. `A*B = B*A`) in addition to being associative, the algorithm can pair in a different pattern.
From theoretical standpoint it makes no difference, but in practice it gives a better memory access pattern:

<img src="http://i.stack.imgur.com/9s8NN.png" width="350">

Not all associative operators are commutative - take matrix multiplication for example.


  [1]: https://en.wikipedia.org/wiki/Associative_property
  [2]: https://en.wikipedia.org/wiki/Commutative_property


## Single-block parallel reduction for commutative operator
The simplest approach to parallel reduction in CUDA is to assign a single block to perform the task:

    static const int arraySize = 10000;
    static const int blockSize = 1024;

    __global__ void sumCommSingleBlock(const int *a, int *out) {
        int idx = threadIdx.x;
        int sum = 0;
        for (int i = idx; i < arraySize; i += blockSize)
            sum += a[i];
        __shared__ int r[blockSize];
        r[idx] = sum;
        __syncthreads();
        for (int size = blockSize/2; size>0; size/=2) { //uniform
            if (idx<size)
                r[idx] += r[idx+size];
            __syncthreads();
        }
        if (idx == 0)
            *out = r[0];
    }

    ...

    sumCommSingleBlock<<<1, blockSize>>>(dev_a, dev_out);

This is most feasable when the data size is not very large (around a few thousants elements).
This usually happens when the reduction is a part of some bigger CUDA program.
If the input matches `blockSize` from the very beginning, the first `for` loop can be completely removed.

Note that in first step, when there are more elements than threads, we add things up completely independently.
Only when the problem is reduced to `blockSize`, the actual parallel reduction triggers.
The same code can be applied to any other commutative, associative operator, such as multiplication, minimum, maximum, etc.

Note that the algorithm can be made faster, for example by using a warp-level parallel reduction.

## Single-block parallel reduction for non-commutative operator
Doing parallel reduction for a non-commutative operator is a bit more involved, compared to commutative version.
In the example we still use a addition over integers for the simplicity sake.
It could be replaced, for example, with matrix multiplication which really is non-commutative.
Note, when doing so, 0 should be replaced by a neutral element of the multiplication - i.e. an identity matrix.

    static const int arraySize = 1000000;
    static const int blockSize = 1024;

    __global__ void sumNoncommSingleBlock(const int *gArr, int *out) {
        int thIdx = threadIdx.x;
        __shared__ int shArr[blockSize*2];
        __shared__ int offset;
        shArr[thIdx] = thIdx<arraySize ? gArr[thIdx] : 0;
        if (thIdx == 0)
            offset = blockSize;
        __syncthreads();
        while (offset < arraySize) { //uniform
            shArr[thIdx + blockSize] = thIdx+offset<arraySize ? gArr[thIdx+offset] : 0;
            __syncthreads();
            if (thIdx == 0)
                offset += blockSize;
            int sum = shArr[2*thIdx] + shArr[2*thIdx+1];
            __syncthreads();
            shArr[thIdx] = sum;
        }
        __syncthreads();
        for (int stride = 1; stride<blockSize; stride*=2) { //uniform
            int arrIdx = thIdx*stride*2;
            if (arrIdx+stride<blockSize)
                shArr[arrIdx] += shArr[arrIdx+stride];
            __syncthreads();
        }   
        if (thIdx == 0)
            *out = shArr[0];
    }

    ...

    sumNoncommSingleBlock<<<1, blockSize>>>(dev_a, dev_out);

In the first while loop executes as long as there are more input elements than threads.
In each iteration, a single reduction is performed and the result is compressed into the first half of the `shArr` array.
The second half is then filled with new data.

Once all data is loaded from `gArr`, the second loop executes.
Now, we no longer compress the result (which costs an extra `__syncthreads()`).
In each step the thread n access the `2*n`-th active element and adds it up with `2*n+1`-th element:

<img src="http://i.stack.imgur.com/jjQvK.png" width="300">

There are many ways to further optimize this simple example, e.g. through warp-level reduction and by removing shared memory bank conflicts.    


## Multi-block parallel reduction for commutative operator
Multi-block approach to parallel reduction in CUDA poses an additional challenge, compared to single-block approach, because blocks are limited in communication.
The idea is to let each block compute a part of the input array, and then have one final block to merge all the partial results.
To do that one can launch two kernels, implicitly creating a grid-wide synchronization point.

    static const int wholeArraySize = 100000000;
    static const int blockSize = 1024;
    static const int gridSize = 24; //this number is hardware-dependent; usually #SM*2 is a good number.

    __global__ void sumCommMultiBlock(const int *gArr, int arraySize, int *gOut) {
        int thIdx = threadIdx.x;
        int gthIdx = thIdx + blockIdx.x*blockSize;
        const int gridSize = blockSize*gridDim.x;
        int sum = 0;
        for (int i = gthIdx; i < arraySize; i += gridSize)
            sum += gArr[i];
        __shared__ int shArr[blockSize];
        shArr[thIdx] = sum;
        __syncthreads();
        for (int size = blockSize/2; size>0; size/=2) { //uniform
            if (thIdx<size)
                shArr[thIdx] += shArr[thIdx+size];
            __syncthreads();
        }
        if (thIdx == 0)
            gOut[blockIdx.x] = shArr[0];
    }
    
    __host__ int sumArray(int* arr) {
        int* dev_arr;
        cudaMalloc((void**)&dev_arr, wholeArraySize * sizeof(int));
        cudaMemcpy(dev_arr, arr, wholeArraySize * sizeof(int), cudaMemcpyHostToDevice);

        int out;
        int* dev_out;
        cudaMalloc((void**)&dev_out, sizeof(int)*gridSize);
        
        sumCommMultiBlock<<<gridSize, blockSize>>>(dev_arr, wholeArraySize, dev_out);
        //dev_out now holds the partial result
        sumCommMultiBlock<<<1, blockSize>>>(dev_out, gridSize, dev_out);
        //dev_out[0] now holds the final result
        cudaDeviceSynchronize();
        
        cudaMemcpy(&out, dev_out, sizeof(int), cudaMemcpyDeviceToHost);
        cudaFree(dev_arr);
        cudaFree(dev_out);
        return out;
    }
        

One ideally wants to launch enough blocks to saturate all multiprocessors on the GPU at full occupancy.
Exceeding this number - in particular, launching as many threads as there are elements in the array - is counter-productive.
Doing so it does not increase the raw compute power anymore, but prevents using the very efficient first loop.

It is also possible to obtain the same result using a single kernel, with the help of [last-block guard][1]:

    static const int wholeArraySize = 100000000;
    static const int blockSize = 1024;
    static const int gridSize = 24;
    
    __device__ bool lastBlock(int* counter) {
        __threadfence(); //ensure that partial result is visible by all blocks
        int last = 0;
        if (threadIdx.x == 0)
            last = atomicAdd(counter, 1);
        return __syncthreads_or(last == gridDim.x-1);
    }    

    __global__ void sumCommMultiBlock(const int *gArr, int arraySize, int *gOut, int* lastBlockCounter) {
        int thIdx = threadIdx.x;
        int gthIdx = thIdx + blockIdx.x*blockSize;
        const int gridSize = blockSize*gridDim.x;
        int sum = 0;
        for (int i = gthIdx; i < arraySize; i += gridSize)
            sum += gArr[i];
        __shared__ int shArr[blockSize];
        shArr[thIdx] = sum;
        __syncthreads();
        for (int size = blockSize/2; size>0; size/=2) { //uniform
            if (thIdx<size)
                shArr[thIdx] += shArr[thIdx+size];
            __syncthreads();
        }
        if (thIdx == 0)
            gOut[blockIdx.x] = shArr[0];
        if (lastBlock(lastBlockCounter)) {
            shArr[thIdx] = thIdx<gridSize ? gOut[thIdx] : 0;
            __syncthreads();
            for (int size = blockSize/2; size>0; size/=2) { //uniform
                if (thIdx<size)
                    shArr[thIdx] += shArr[thIdx+size];
                __syncthreads();
            }
            if (thIdx == 0)
                gOut[0] = shArr[0];            
        }
    }

    __host__ int sumArray(int* arr) {
        int* dev_arr;
        cudaMalloc((void**)&dev_arr, wholeArraySize * sizeof(int));
        cudaMemcpy(dev_arr, arr, wholeArraySize * sizeof(int), cudaMemcpyHostToDevice);

        int out;
        int* dev_out;
        cudaMalloc((void**)&dev_out, sizeof(int)*gridSize);
        
        int* dev_lastBlockCounter;
        cudaMalloc((void**)&dev_lastBlockCounter, sizeof(int));
        cudaMemset(dev_lastBlockCounter, 0, sizeof(int));
        
        sumCommMultiBlock<<<gridSize, blockSize>>>(dev_arr, wholeArraySize, dev_out, dev_lastBlockCounter);
        cudaDeviceSynchronize();
        
        cudaMemcpy(&out, dev_out, sizeof(int), cudaMemcpyDeviceToHost);
        cudaFree(dev_arr);
        cudaFree(dev_out);
        return out;
    }    
    
Note that the kernel can be made faster, for example by using a warp-level parallel reduction.


  [1]: https://www.wikiod.com/cuda/inter-block-communication#Last-block guard

## Multi-block parallel reduction for noncommutative operator
Multi-block approach to parallel reduction is very similar to the single-block approach.
The global input array must be split into sections, each reduced by a single block.
When a partial result from each block is obtained, one final block reduces these to obtain the final result.

- `sumNoncommSingleBlock` is explained more in detail in the Single-block reduction example.
- [`lastBlock`][1] accepts only the last block reaching it. If you want to avoid this, you can split the kernel into two separate calls.


    static const int wholeArraySize = 100000000;
    static const int blockSize = 1024;
    static const int gridSize = 24; //this number is hardware-dependent; usually #SM*2 is a good number.

    __device__ bool lastBlock(int* counter) {
        __threadfence(); //ensure that partial result is visible by all blocks
        int last = 0;
        if (threadIdx.x == 0)
            last = atomicAdd(counter, 1);
        return __syncthreads_or(last == gridDim.x-1);
    }
    
    __device__ void sumNoncommSingleBlock(const int* gArr, int arraySize, int* out) {
        int thIdx = threadIdx.x;
        __shared__ int shArr[blockSize*2];
        __shared__ int offset;
        shArr[thIdx] = thIdx<arraySize ? gArr[thIdx] : 0;
        if (thIdx == 0)
            offset = blockSize;
        __syncthreads();
        while (offset < arraySize) { //uniform
            shArr[thIdx + blockSize] = thIdx+offset<arraySize ? gArr[thIdx+offset] : 0;
            __syncthreads();
            if (thIdx == 0)
                offset += blockSize;
            int sum = shArr[2*thIdx] + shArr[2*thIdx+1];
            __syncthreads();
            shArr[thIdx] = sum;
        }
        __syncthreads();
        for (int stride = 1; stride<blockSize; stride*=2) { //uniform
            int arrIdx = thIdx*stride*2;
            if (arrIdx+stride<blockSize)
                shArr[arrIdx] += shArr[arrIdx+stride];
            __syncthreads();
        }   
        if (thIdx == 0)
            *out = shArr[0];
    }
    
    __global__ void sumNoncommMultiBlock(const int* gArr, int* out, int* lastBlockCounter) {
        int arraySizePerBlock = wholeArraySize/gridSize;
        const int* gArrForBlock = gArr+blockIdx.x*arraySizePerBlock;
        int arraySize = arraySizePerBlock;
        if (blockIdx.x == gridSize-1)
            arraySize = wholeArraySize - blockIdx.x*arraySizePerBlock;
        sumNoncommSingleBlock(gArrForBlock, arraySize, &out[blockIdx.x]);
        if (lastBlock(lastBlockCounter))
            sumNoncommSingleBlock(out, gridSize, out);    
    }
    
One ideally wants to launch enough blocks to saturate all multiprocessors on the GPU at full occupancy.
Exceeding this number - in particular, launching as many threads as there are elements in the array - is counter-productive.
Doing so it does not increase the raw compute power anymore, but prevents using the very efficient first loop.


  [1]: https://www.wikiod.com/cuda/inter-block-communication#Last-block guard

## Single-warp parallel reduction for commutative operator
Sometimes the reduction has to be performed on a very small scale, as a part of a bigger CUDA kernel.
Suppose for example, that the input data has exactly 32 elements - the number of threads in a warp.
In such scenario a single warp can be assigned to perform the reduction.
Given that warp executes in a perfect sync, many `__syncthreads()` instructions can be removed - when compared to a block-level reduction.

    static const int warpSize = 32;

    __device__ int sumCommSingleWarp(volatile int* shArr) {
        int idx = threadIdx.x % warpSize; //the lane index in the warp
        if (idx<16) shArr[idx] += shArr[idx+16];
        if (idx<8) shArr[idx] += shArr[idx+8];
        if (idx<4) shArr[idx] += shArr[idx+4];
        if (idx<2) shArr[idx] += shArr[idx+2];
        if (idx==0) shArr[idx] += shArr[idx+1];
        return shArr[0];
    }
    
`shArr` is preferably an array in shared memory. The value should be the same for all threads in the warp.
If `sumCommSingleWarp` is called by multiple warps, `shArr` should be different between warps (same within each warp).

The argument `shArr` is marked as `volatile` to ensure that operations on the array are actually performed where indicated.
Otherwise, the repetetive assignment to `shArr[idx]` may be optimized as an assignment to a register, with only final assigment being an actual store to `shArr`.
When that happens, the immediate assignments are not visible to other threads, yielding incorrect results.
Note, that you can pass a normal non-volatile array as an argument of volatile one, same as when you pass non-const as a const parameter.

If one does not care about the contents of `shArr[1..31]` after the reduction, one can simplify the code even further:

    static const int warpSize = 32;
    
    __device__ int sumCommSingleWarp(volatile int* shArr) {
        int idx = threadIdx.x % warpSize; //the lane index in the warp
        if (idx<16) {
          shArr[idx] += shArr[idx+16];
          shArr[idx] += shArr[idx+8];
          shArr[idx] += shArr[idx+4];
          shArr[idx] += shArr[idx+2];
          shArr[idx] += shArr[idx+1];
        }
        return shArr[0];
    }

In this setup we removed many `if` conditions. The extra threads perform some unnecessary additions, but we no longer care about the contents they produce.
Since warps execute in SIMD mode we do not actually save on time by having those threads doing nothing.
On the other hand, evaluating the conditions does take relatively big amount of time, since the body of these `if` statements are so small.
The initial `if` statement can be removed as well if `shArr[32..47]` are padded with 0.

The warp-level reduction can be used to boost the block-level reduction as well:

    __global__ void sumCommSingleBlockWithWarps(const int *a, int *out) {
        int idx = threadIdx.x;
        int sum = 0;
        for (int i = idx; i < arraySize; i += blockSize)
            sum += a[i];
        __shared__ int r[blockSize];
        r[idx] = sum;
        sumCommSingleWarp(&r[idx & ~(warpSize-1)]);
        __syncthreads();
        if (idx<warpSize) { //first warp only
            r[idx] = idx*warpSize<blockSize ? r[idx*warpSize] : 0;
            sumCommSingleWarp(r);
            if (idx == 0)
                *out = r[0];
        }
    }

The argument `&r[idx & ~(warpSize-1)]` is basically `r + warpIdx*32`.
This effectively splits the `r` array into chunks of 32 elements, and each chunk is assigned to separate warp.


## Single-warp parallel reduction for noncommutative operator
Sometimes the reduction has to be performed on a very small scale, as a part of a bigger CUDA kernel.
Suppose for example, that the input data has exactly 32 elements - the number of threads in a warp.
In such scenario a single warp can be assigned to perform the reduction.
Given that warp executes in a perfect sync, many `__syncthreads()` instructions can be removed - when compared to a block-level reduction.

    static const int warpSize = 32;

    __device__ int sumNoncommSingleWarp(volatile int* shArr) {
        int idx = threadIdx.x % warpSize; //the lane index in the warp
        if (idx%2 == 0) shArr[idx] += shArr[idx+1];
        if (idx%4 == 0) shArr[idx] += shArr[idx+2];
        if (idx%8 == 0) shArr[idx] += shArr[idx+4];
        if (idx%16 == 0) shArr[idx] += shArr[idx+8];
        if (idx == 0) shArr[idx] += shArr[idx+16];
        return shArr[0];
    }

`shArr` is preferably an array in shared memory. The value should be the same for all threads in the warp.
If `sumCommSingleWarp` is called by multiple warps, `shArr` should be different between warps (same within each warp).
    
The argument `shArr` is marked as `volatile` to ensure that operations on the array are actually performed where indicated.
Otherwise, the repetetive assignment to `shArr[idx]` may be optimized as an assignment to a register, with only final assigment being an actual store to `shArr`.
When that happens, the immediate assignments are not visible to other threads, yielding incorrect results.
Note, that you can pass a normal non-volatile array as an argument of volatile one, same as when you pass non-const as a const parameter.    
    
If one does not care about the final contents of `shArr[1..31]` and can pad `shArr[32..47]` with zeros, one can simplify the above code:

    static const int warpSize = 32;
    
    __device__ int sumNoncommSingleWarpPadded(volatile int* shArr) {
        //shArr[32..47] == 0
        int idx = threadIdx.x % warpSize; //the lane index in the warp
        shArr[idx] += shArr[idx+1];
        shArr[idx] += shArr[idx+2];
        shArr[idx] += shArr[idx+4];
        shArr[idx] += shArr[idx+8];
        shArr[idx] += shArr[idx+16];
        return shArr[0];
    }

In this setup we removed all `if` conditions, which constitute about the half of the instructions.
The extra threads perform some unnecessary additions, storing the result into cells of `shArr` that ultimately have no impact on the final result.
Since warps execute in SIMD mode we do not actually save on time by having those threads doing nothing.

## Single-warp parallel reduction using registers only
Typically, reduction is performed on global or shared array.
However, when the reduction is performed on a very small scale, as a part of a bigger CUDA kernel, it can be performed with a single warp.
When that happens, on Keppler or higher architectures (CC>=3.0), it is possible to use warp-shuffle functions to avoid using shared memory at all.

Suppose for example, that each thread in a warp holds a single input data value.
All threads together have 32 elements, that we need to sum up (or perform other associative operation)

    __device__ int sumSingleWarpReg(int value) {
        value += __shfl_down(value, 1);
        value += __shfl_down(value, 2);
        value += __shfl_down(value, 4);
        value += __shfl_down(value, 8);
        value += __shfl_down(value, 16);
        return __shfl(value,0);
    }
    
This version works for both commutative and non-commutative operators.


